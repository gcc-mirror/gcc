------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             L I B . W R I T                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2013, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains the routines for writing the library information

package Lib.Writ is

   -----------------------------------
   -- Format of Library Information --
   -----------------------------------

   --  This section describes the format of the library information that is
   --  associated with object files. The exact method of this association is
   --  potentially implementation dependent and is described and implemented in
   --  package ali. From the point of view of the description here, all we need
   --  to know is that the information is represented as a string of characters
   --  that is somehow associated with an object file, and can be retrieved. If
   --  no library information exists for a given object file, then we take this
   --  as equivalent to the non-existence of the object file, as if source file
   --  has not been previously compiled.

   --  The library information is written as a series of lines of the form:

   --    Key_Character parameter parameter ...

   --  The following sections describe the format of these lines in detail

   --------------------------------------
   -- Making Changes to the ALI Format --
   --------------------------------------

   --  A number of tools use ali.adb to parse ali files. This means that
   --  changes to this format can cause old versions of these tools to be
   --  incompatible with new versions of the compiler. Any changes to ali file
   --  formats must be carefully evaluated to understand any such possible
   --  conflicts, and in particular, it is very undesirable to create conflicts
   --  between older versions of GPS and newer versions of the compiler.

   --  If the following guidelines are respected, downward compatibility
   --  problems (old tools reading new ali files) should be minimized:

   --    The basic key character format must be kept

   --    The V line must be the first line, this is checked by ali.adb even in
   --    Ignore_Errors mode, and is used to verify that the file at hand is
   --    indeed likely intended to be an ali file.

   --    The P line must be present, though may be modified in contents
   --    according to remaining guidelines. Again, ali.adb assumes the P
   --    line is present even in Ignore_Errors mode.

   --    New modifiers can generally be added (in particular adding new two
   --    letter modifiers to the P or U lines is always safe)

   --    Adding entirely new lines (with a new key letter) to the ali file is
   --    always safe, at any point (other than before the V line), since such
   --    lines will be ignored.

   --  Following the guidelines in this section should ensure that this problem
   --  is minimized and that old tools will be able to deal successfully with
   --  new ali formats. Note that this does not apply to the compiler itself,
   --  which always requires consistency between the ali files and the binder.
   --  That is because one of the main functions of the binder is to ensure
   --  consistency of the partition, and this can be compromised if the ali
   --  files are inconsistent.

   ------------------
   -- Header Lines --
   ------------------

   --  The initial header lines in the file give information about the
   --  compilation environment, and identify other special information such as
   --  main program parameters.

   --  ----------------
   --  -- V  Version --
   --  ----------------

   --    V "xxxxxxxxxxxxxxxx"
   --
   --      This line indicates the library output version, as defined in
   --      Gnatvsn. It ensures that separate object modules of a program are
   --      consistent. It has to be changed if anything changes which would
   --      affect successful binding of separately compiled modules. Examples
   --      of such changes are modifications in the format of the library info
   --      described in this package, or modifications to calling sequences, or
   --      to the way that data is represented.

   --    Note: the V line absolutely must be the first line, and no change
   --    to the ALI format should change this, since even in Ignore_Errors
   --    mode, Scan_ALI insists on finding a V line.

   --  ---------------------
   --  -- M  Main Program --
   --  ---------------------

   --    M type [priority] [T=time-slice] [AB] [C=cpu] W=?

   --      This line appears only if the main unit for this file is suitable
   --      for use as a main program. The parameters are:

   --        type

   --          P for a parameterless procedure
   --          F for a function returning a value of integral type
   --            (used for writing a main program returning an exit status)

   --        priority

   --          Present only if there was a valid pragma Priority in the
   --          corresponding unit to set the main task priority. It is an
   --          unsigned decimal integer.

   --        T=time-slice

   --          Present only if there was a valid pragma Time_Slice in the
   --          corresponding unit. It is an unsigned decimal integer in the
   --          range 0 .. 10**9 giving the time slice value in units of
   --          milliseconds. The actual significance of this parameter is
   --          target dependent.

   --        AB

   --          Present if there is an allocator in the body of the procedure
   --          after the BEGIN. This will be a violation of the restriction
   --          No_Allocators_After_Elaboration if it is present, and this
   --          unit is used as a main program (only the binder can find the
   --          violation, since only the binder knows the main program).

   --        C=cpu

   --          Present only if there was a valid pragma CPU in the
   --          corresponding unit to set the main task affinity. It is an
   --          unsigned decimal integer.

   --        W=?

   --          This parameter indicates the wide character encoding method used
   --          when compiling the main program file. The ? character is the
   --          single character used in the -gnatW? switch. This is used to
   --          provide the default wide-character encoding for Wide_Text_IO
   --          files.

   --  -----------------
   --  -- A  Argument --
   --  -----------------

   --    A argument

   --      One of these lines appears for each of the arguments present in the
   --      call to the gnat1 program. This can be used if it is necessary to
   --      reconstruct this call (e.g. for fix and continue).

   --  -------------------
   --  -- P  Parameters --
   --  -------------------

   --    P <<parameters>>

   --      Indicates various information that applies to the compilation of the
   --      corresponding source file. Parameters is a sequence of zero or more
   --      two letter codes that indicate configuration pragmas and other
   --      parameters that apply:
   --
   --      The arguments are as follows:
   --
   --         CE   Compilation errors. If this is present it means that the ali
   --              file resulted from a compilation with the -gnatQ switch set,
   --              and illegalities were detected. The ali file contents may
   --              not be completely reliable, but the format will be correct
   --              and complete. Note that NO is always present if CE is
   --              present.
   --
   --         DB   Detect_Blocking pragma is in effect for all units in this
   --              file.
   --
   --         Ex   A valid Partition_Elaboration_Policy pragma applies to all
   --              the units in this file, where x is the first character
   --              (upper case) of the policy name (e.g. 'C' for Concurrent).
   --
   --         FD   Configuration pragmas apply to all the units in this file
   --              specifying a possibly non-standard floating point format
   --              (VAX float with Long_Float using D_Float).
   --
   --         FG   Configuration pragmas apply to all the units in this file
   --              specifying a possibly non-standard floating point format
   --              (VAX float with Long_Float using G_Float).
   --
   --         FI   Configuration pragmas apply to all the units in this file
   --              specifying a possibly non-standard floating point format
   --              (IEEE Float).
   --
   --         Lx   A valid Locking_Policy pragma applies to all the units in
   --              this file, where x is the first character (upper case) of
   --              the policy name (e.g. 'C' for Ceiling_Locking).
   --
   --         NO   No object. This flag indicates that the units in this file
   --              were not compiled to produce an object. This can occur as a
   --              result of the use of -gnatc, or if no object can be produced
   --              (e.g. when a package spec is compiled instead of the body,
   --              or a subunit on its own).
   --
   --         NR   No_Run_Time. Indicates that a pragma No_Run_Time applies
   --              to all units in the file.
   --
   --         NS   Normalize_Scalars pragma in effect for all units in
   --              this file.
   --
   --         Qx   A valid Queueing_Policy pragma applies to all the units
   --              in this file, where x is the first character (upper case)
   --              of the policy name (e.g. 'P' for Priority_Queueing).
   --
   --         SL   Indicates that the unit is an Interface to a Standalone
   --              Library. Note that this indication is never given by the
   --              compiler, but is added by the Project Manager in gnatmake
   --              when an Interface ALI file is copied to the library
   --              directory.

   --         SS   This unit references System.Secondary_Stack (that is,
   --              the unit makes use of the secondary stack facilities).
   --
   --         Tx   A valid Task_Dispatching_Policy pragma applies to all
   --              the units in this file, where x is the first character
   --              (upper case) of the corresponding policy name (e.g. 'F'
   --              for FIFO_Within_Priorities).
   --
   --         UA  Unreserve_All_Interrupts pragma was processed in one or
   --             more units in this file
   --
   --         ZX  Units in this file use zero-cost exceptions and have
   --             generated exception tables. If ZX is not present, the
   --             longjmp/setjmp exception scheme is in use.
   --
   --      Note that language defined units never output policy (Lx, Tx, Qx)
   --      parameters. Language defined units must correctly handle all
   --      possible cases. These values are checked for consistency by the
   --      binder and then copied to the generated binder output file.

   --    Note: The P line must be present. Even in Ignore_Errors mode, Scan_ALI
   --    insists on finding a P line. So if changes are made to the ALI format,
   --    they should not include removing the P line!

   --  ---------------------
   --  -- R  Restrictions --
   --  ---------------------

   --  There are two forms for R lines, positional and named. The positional
   --  notation is now considered obsolescent, it is not generated by the most
   --  recent versions of the compiler except under control of the debug switch
   --  -gnatdR, but is still recognized by the binder.

   --  The recognition by the binder is to ease the transition, and better deal
   --  with some cases of inconsistent builds using incompatible versions of
   --  the compiler and binder. The named notation is the current preferred
   --  approach.

   --  Note that R lines are generated using the information in unit Rident,
   --  and intepreted by the binder using the information in System.Rident.
   --  Normally these two units should be effectively identical. However in
   --  some cases of inconsistent builds, they may be different. This may lead
   --  to binder diagnostics, which can be suppressed using the -C switch for
   --  the binder, which results in ignoring unrecognized restrictions in the
   --  ali files.

   --  ---------------------------------------
   --  -- R  Restrictions (Positional Form) --
   --  ---------------------------------------

   --  The first R line records the status of restrictions generated by pragma
   --  Restrictions encountered, as well as information on what the compiler
   --  has been able to determine with respect to restrictions violations.
   --  The format is:

   --    R <<restriction-characters>> <<restriction-param-id-entries>>

   --      The first parameter is a string of characters that records
   --      information regarding restrictions that do not take parameter not
   --      take parameter values. It is a string of characters, one character
   --      for each value (in order) in All_Boolean_Restrictions. There are
   --      three possible settings for each restriction:

   --        r   Restricted. Unit was compiled under control of a pragma
   --            Restrictions for the corresponding restriction. In this case
   --            the unit certainly does not violate the Restriction, since
   --            this would have been detected by the compiler.

   --        n   Not used. The unit was not compiled under control of a pragma
   --            Restrictions for the corresponding restriction, and does not
   --            make any use of the referenced feature.

   --        v   Violated. The unit was not compiled under control of a pragma
   --            Restrictions for the corresponding restriction, and it does
   --            indeed use the referenced feature.

   --      This information is used in the binder to check consistency, i.e. to
   --      detect cases where one unit has "r" and another unit has "v", which
   --      is not permitted, since these restrictions are partition-wide.

   --  The second parameter, which immediately follows the first (with no
   --  separating space) gives restriction information for identifiers for
   --  which a parameter is given.

   --      The parameter is a string of entries, one for each value in
   --      Restrict.All_Parameter_Restrictions. Each entry has two components
   --      in sequence, the first indicating whether or not there is a
   --      restriction, and the second indicating whether or not the compiler
   --      detected violations. In the boolean case it is not necessary to
   --      separate these, since if a restriction is set, and violated, that is
   --      an error. But in the parameter case, this is not true. For example,
   --      we can have a unit with a pragma Restrictions (Max_Tasks => 4),
   --      where the compiler can detect that there are exactly three tasks
   --      declared. Both of these pieces of information must be passed to the
   --      binder. The parameter of 4 is important in case the total number of
   --      tasks in the partition is greater than 4. The parameter of 3 is
   --      important in case some other unit has a restrictions pragma with
   --      Max_Tasks=>2.

   --      The component for the presence of restriction has one of two
   --      possible forms:

   --         n   No pragma for this restriction is present in the set of units
   --             for this ali file.

   --         rN  At least one pragma for this restriction is present in the
   --             set of units for this ali file. The value N is the minimum
   --             parameter value encountered in any such pragma. N is in the
   --             range of Integer (a value larger than N'Last causes the
   --             pragma to be ignored).

   --      The component for the violation detection has one of three
   --      possible forms:

   --         n   No violations were detected by the compiler

   --         vN  A violation was detected. N is either the maximum or total
   --             count of violations (depending on the checking type) in all
   --             the units represented by the ali file). Note that this
   --             setting is only allowed for restrictions that are in
   --             Checked_[Max|Sum]_Parameter_Restrictions. The value here is
   --             known to be exact by the compiler and is in the range of
   --             Natural.

   --         vN+ A violation was detected. The compiler cannot determine
   --             the exact count of violations, but it is at least N.

   --      There are no spaces within the parameter string, so the entry
   --      described above in the header of this section for Max_Tasks would
   --      appear as the string r4v3.

   --      Note: The restrictions line is required to be present. Even in
   --      Ignore_Errors mode, Scan_ALI expects to find an R line and will
   --      signal a fatal error if it is missing. This means that future
   --      changes to the ALI file format must retain the R line.

   --  ----------------------------------
   --  -- R  Restrictions (Named Form) --
   --  ----------------------------------

   --  The first R line for named form announces that named notation will be
   --  used, and also assures that there is at least one R line present, which
   --  makes parsing of ali files simpler. A blank line preceds the RN line.

   --  RN

   --  In named notation, the restrictions are given as a series of lines, one
   --  per retrictions that is specified or violated (no information is present
   --  for restrictions that are not specified or violated). In the following
   --  name is the name of the restriction in all upper case.

   --  For boolean restrictions, we have only two possibilities. A restrictions
   --  pragma is present, or a violation is detected:

   --  RR name

   --    A restriction pragma is present for the named boolean restriction.
   --    No violations were detected by the compiler (or the unit in question
   --    would have been found to be illegal).

   --  RV name

   --    No restriction pragma is present for the named boolean restriction.
   --    However, the compiler did detect one or more violations of this
   --    restriction, which may require a binder consistency check. Note that
   --    one case of a violation is the use of a Restriction_Set attribute for
   --    the restriction that yielded False.

   --  For the case of restrictions that take a parameter, we need both the
   --  information from pragma if present, and the actual information about
   --  what possible violations occur. For example, we can have a unit with
   --  a pragma Restrictions (Max_Tasks => 4), where the compiler can detect
   --  that there are exactly three tasks declared. Both of these pieces
   --  of information must be passed to the binder. The parameter of 4 is
   --  important in case the total number of tasks in the partition is greater
   --  than 4. The parameter of 3 is important in case some other unit has a
   --  restrictions pragma with Max_Tasks=>2.

   --  RR name=N

   --    A restriction pragma is present for the named restriction which is
   --    one of the restrictions taking a parameter. The value N (a decimal
   --    integer) is the value given in the restriction pragma.

   --  RV name=N

   --    A restriction pragma may or may not be present for the restriction
   --    given by name (one of the restrictions taking a parameter). But in
   --    either case, the compiler detected possible violations. N (a decimal
   --    integer) is the maximum or total count of violations (depending
   --    on the checking type) in all the units represented by the ali file).
   --    The value here is known to be exact by the compiler and is in the
   --    range of Natural. Note that if an RR line is present for the same
   --    restriction, then the value in the RV line cannot exceed the value
   --    in the RR line (since otherwise the compiler would have detected a
   --    violation of the restriction).

   --  RV name=N+

   --    Similar to the above, but the compiler cannot determine the exact
   --    count of violations, but it is at least N.

   --  -------------------------------------------------
   --  -- R  Restrictions (No_Dependence Information) --
   --  -------------------------------------------------

   --  Subsequent R lines are present only if pragma Restriction No_Dependence
   --  is used. There is one such line for each such pragma appearing in the
   --  extended main unit. The format is:

   --    R unit_name

   --      Here the unit name is in all lower case. The components of the unit
   --      name are separated by periods. The names themselves are in encoded
   --      form, as documented in Namet.

   --  -------------------------
   --  -- I  Interrupt States --
   --  -------------------------

   --    I interrupt-number interrupt-state line-number

   --      This line records information from an Interrupt_State pragma. There
   --      is one line for each separate pragma, and if no such pragmas are
   --      used, then no I lines are present.

   --      The interrupt-number is an unsigned positive integer giving the
   --      value of the interrupt as defined in Ada.Interrupts.Names.

   --      The interrupt-state is one of r/s/u for Runtime/System/User

   --      The line number is an unsigned decimal integer giving the line
   --      number of the corresponding Interrupt_State pragma. This is used
   --      in consistency messages.

   --  --------------------------------------
   --  -- S  Priority Specific Dispatching --
   --  --------------------------------------

   --    S policy_identifier first_priority last_priority line-number

   --      This line records information from a Priority_Specific_Dispatching
   --      pragma. There is one line for each separate pragma, and if no such
   --      pragmas are used, then no S lines are present.

   --      The policy_identifier is the first character (upper case) of the
   --      corresponding policy name (e.g. 'F' for FIFO_Within_Priorities).

   --      The first_priority and last_priority fields define the range of
   --      priorities to which the specified dispatching policy apply.

   --      The line number is an unsigned decimal integer giving the line
   --      number of the corresponding Priority_Specific_Dispatching pragma.
   --      This is used in consistency messages.

   ----------------------------
   -- Compilation Unit Lines --
   ----------------------------

   --  Following these header lines, a set of information lines appears for
   --  each compilation unit that appears in the corresponding object file. In
   --  particular, when a package body or subprogram body is compiled, there
   --  will be two sets of information, one for the spec and one for the body,
   --  with the entry for the body appearing first. This is the only case in
   --  which a single ALI file contains more than one unit (in particular note
   --  that subunits do *not* count as compilation units for this purpose, and
   --  generate no library information, since they are inlined).

   --  --------------------
   --  -- U  Unit Header --
   --  --------------------

   --  The lines for each compilation unit have the following form

   --    U unit-name source-name version <<attributes>>
   --
   --      This line identifies the unit to which this section of the library
   --      information file applies. The first three parameters are the unit
   --      name in internal format, as described in package Uname, and the name
   --      of the source file containing the unit.
   --
   --      Version is the version given as eight hexadecimal characters with
   --      upper case letters. This value is the exclusive or of the source
   --      checksums of the unit and all its semantically dependent units.
   --
   --      The <<attributes>> are a series of two letter codes indicating
   --      information about the unit:
   --
   --         BD  Unit does not have pragma Elaborate_Body, but the elaboration
   --             circuit has determined that it would be a good idea if this
   --             pragma were present, since the body of the package contains
   --             elaboration code that modifies one or more variables in the
   --             visible part of the package. The binder will try, but does
   --             not promise, to keep the elaboration of the body close to
   --             the elaboration of the spec.
   --
   --         DE  Dynamic Elaboration. This unit was compiled with the dynamic
   --             elaboration model, as set by either the -gnatE switch or
   --             pragma Elaboration_Checks (Dynamic).
   --
   --         EB  Unit has pragma Elaborate_Body, or is a generic instance that
   --             has a body. Set for instances because RM 12.3(20) requires
   --             that the body be immediately elaborated after the spec (we
   --             would normally do that anyway, because elaborate spec and
   --             body together whenever possible, and for an instance it is
   --             always possible; however setting EB ensures that this is done
   --             even when using the -p gnatbind switch).
   --
   --         EE  Elaboration entity is present which must be set true when
   --             the unit is elaborated. The name of the elaboration entity is
   --             formed from the unit name in the usual way. If EE is present,
   --             then this boolean must be set True as part of the elaboration
   --             processing routine generated by the binder. Note that EE can
   --             be set even if NE is set. This happens when the boolean is
   --             needed solely for checking for the case of access before
   --             elaboration.
   --
   --         GE  Unit is a generic declaration, or corresponding body
   --
   --         IL  Unit source uses a style with identifiers in all lower-case
   --         IU  (IL) or all upper case (IU). If the standard mixed-case usage
   --             is detected, or the compiler cannot determine the style, then
   --             no I parameter will appear.
   --
   --         IS  Initialize_Scalars pragma applies to this unit, or else there
   --             is at least one use of the Invalid_Value attribute.
   --
   --         KM  Unit source uses a style with keywords in mixed case (KM)
   --         KU  or all upper case (KU). If the standard lower-case usage is
   --             is detected, or the compiler cannot determine the style, then
   --             no K parameter will appear.
   --
   --         NE  Unit has no elaboration routine. All subprogram bodies and
   --             specs are in this category. Package bodies and specs may or
   --             may not have NE set, depending on whether or not elaboration
   --             code is required. Set if N_Compilation_Unit node has flag
   --             Has_No_Elaboration_Code set.
   --
   --         OL   The units in this file are compiled with a local pragma
   --              Optimize_Alignment, so no consistency requirement applies
   --              to these units. All internal units have this status since
   --              they have an automatic default of Optimize_Alignment (Off).
   --
   --         OO   Optimize_Alignment (Off) is the default setting for all
   --              units in this file. All files in the partition that specify
   --              a default must specify the same default.
   --
   --         OS   Optimize_Alignment (Space) is the default setting for all
   --              units in this file. All files in the partition that specify
   --              a default must specify the same default.
   --
   --         OT   Optimize_Alignment (Time) is the default setting for all
   --              units in this file. All files in the partition that specify
   --              a default must specify the same default.
   --
   --         PF  The unit has a library-level (package) finalizer
   --
   --         PK  Unit is package, rather than a subprogram
   --
   --         PU  Unit has pragma Pure
   --
   --         PR  Unit has pragma Preelaborate
   --
   --         RA  Unit declares a Remote Access to Class-Wide (RACW) type
   --
   --         RC  Unit has pragma Remote_Call_Interface
   --
   --         RT  Unit has pragma Remote_Types
   --
   --         SP  Unit has pragma Shared_Passive.
   --
   --         SU  Unit is a subprogram, rather than a package
   --
   --      The attributes may appear in any order, separated by spaces.

   --  -----------------------------
   --  -- W, Y and Z Withed Units --
   --  -----------------------------

   --  Following each U line, is a series of lines of the form

   --    W unit-name [source-name lib-name] [E] [EA] [ED] [AD]
   --      or
   --    Y unit-name [source-name lib-name] [E] [EA] [ED] [AD]
   --      or
   --    Z unit-name [source-name lib-name] [E] [EA] [ED] [AD]
   --
   --      One W line is present for each unit that is mentioned in an explicit
   --      non-limited with clause by the current unit. One Y line is present
   --      for each unit that is mentioned in an explicit limited with clause
   --      by the current unit. One Z line is present for each unit that is
   --      only implicitly withed by the current unit. The first parameter is
   --      the unit name in internal format. The second parameter is the file
   --      name of the file that must be compiled to compile this unit. It is
   --      usually the file for the body, except for packages which have no
   --      body. For units that need a body, if the source file for the body
   --      cannot be found, the file name of the spec is used instead. The
   --      third parameter is the file name of the library information file
   --      that contains the results of compiling this unit. The optional
   --      modifiers are used as follows:
   --
   --        E   pragma Elaborate applies to this unit
   --
   --        EA  pragma Elaborate_All applies to this unit
   --
   --        ED  Elaborate_Desirable set for this unit, which means that there
   --            is no Elaborate, but the analysis suggests that Program_Error
   --            may be raised if the Elaborate conditions cannot be satisfied.
   --            The binder will attempt to treat ED as E if it can.
   --
   --        AD  Elaborate_All_Desirable set for this unit, which means that
   --            there is no Elaborate_All, but the analysis suggests that
   --            Program_Error may be raised if the Elaborate_All conditions
   --            cannot be satisfied. The binder will attempt to treat AD as
   --            EA if it can.
   --
   --      The parameter source-name and lib-name are omitted for the case of a
   --      generic unit compiled with earlier versions of GNAT which did not
   --      generate object or ali files for generics.
   --
   --      The parameter source-name and lib-name are also omitted for the W
   --      lines that result from use of a Restriction_Set attribute which gets
   --      a result of False from a No_Dependence check, in the case where the
   --      unit is not in the semantic closure. In such a case, the bare W
   --      line is generated, but no D (dependency) line. This will make the
   --      binder do the consistency check, but not include the unit in the
   --      partition closure (unless it is properly With'ed somewhere).

   --  -----------------------
   --  -- L  Linker_Options --
   --  -----------------------

   --  Following the W lines (if any, or the U line if not), are an optional
   --  series of lines that indicates the usage of the pragma Linker_Options in
   --  the associated unit. For each appearance of a pragma Linker_Options (or
   --  Link_With) in the unit, a line is present with the form:

   --    L "string"

   --      where string is the string from the unit line enclosed in quotes.
   --      Within the quotes the following can occur:

   --        c    graphic characters in range 20-7E other than " or {
   --        ""   indicating a single " character
   --        {hh} indicating a character whose code is hex hh (0-9,A-F)
   --        {00} [ASCII.NUL] is used as a separator character
   --             to separate multiple arguments of a single
   --             Linker_Options pragma.

   --      For further details, see Stringt.Write_String_Table_Entry. Note that
   --      wide characters in the form {hhhh} cannot be produced, since pragma
   --      Linker_Option accepts only String, not Wide_String.

   --      The L lines are required to appear in the same order as the
   --      corresponding Linker_Options (or Link_With) pragmas appear in the
   --      source file, so that this order is preserved by the binder in
   --      constructing the set of linker arguments.

   --  --------------
   --  -- N  Notes --
   --  --------------

   --  The final section of unit-specific lines contains notes which record
   --  annotations inserted in source code for processing by external tools
   --  using pragmas. For each occurrence of any of these pragmas, a line is
   --  generated with the following syntax:

   --    N x<sloc> [<arg_id>:]<arg> ...

   --      x is one of:
   --        A  pragma Annotate
   --        C  pragma Comment
   --        I  pragma Ident
   --        T  pragma Title
   --        S  pragma Subtitle

   --      <sloc> is the source location of the pragma in line:col format

   --      Successive entries record the pragma_argument_associations.

   --        If a pragma argument identifier is present, the entry is prefixed
   --        with the pragma argument identifier <arg_id> followed by a colon.

   --        <arg> represents the pragma argument, and has the following
   --        conventions:

   --          - identifiers are output verbatim
   --          - static string expressions are output as literals encoded as
   --            for L lines
   --          - static integer expressions are output as decimal literals
   --          - any other expression is replaced by the placeholder "<expr>"

   ---------------------
   -- Reference Lines --
   ---------------------

   --  The reference lines contain information about references from any of the
   --  units in the compilation (including body version and version attributes,
   --  linker options pragmas and source dependencies).

   --  ------------------------------------
   --  -- E  External Version References --
   --  ------------------------------------

   --  One of these lines is present for each use of 'Body_Version or 'Version
   --  in any of the units of the compilation. These are used by the linker to
   --  determine which version symbols must be output. The format is simply:

   --    E name

   --  where name is the external name, i.e. the unit name with either a S or a
   --  B for spec or body version referenced (Body_Version always references
   --  the body, Version references the Spec, except in the case of a reference
   --  to a subprogram with no separate spec). Upper half and wide character
   --  codes are encoded using the same method as in Namet (Uhh for upper half,
   --  Whhhh for wide character, where hh are hex digits).

   --  ---------------------
   --  -- D  Dependencies --
   --  ---------------------

   --  The dependency lines indicate the source files on which the compiled
   --  units depend. This is used by the binder for consistency checking.
   --  These lines are also referenced by the cross-reference information.

   --    D source-name time-stamp checksum [subunit-name] line:file-name

   --      The time-stamp field contains the time stamp of the corresponding
   --      source file. See types.ads for details on time stamp representation.

   --      The checksum is an 8-hex digit representation of the source file
   --      checksum, with letters given in lower case.

   --      The subunit name is present only if the dependency line is for a
   --      subunit. It contains the fully qualified name of the subunit in all
   --      lower case letters.

   --      The line:file-name entry is present only if a Source_Reference
   --      pragma appeared in the source file identified by source-name. In
   --      this case, it gives the information from this pragma. Note that this
   --      allows cross-reference information to be related back to the
   --      original file. Note: the reason the line number comes first is that
   --      a leading digit immediately identifies this as a Source_Reference
   --      entry, rather than a subunit-name.

   --      A line number of zero for line: in this entry indicates that there
   --      is more than one source reference pragma. In this case, the line
   --      numbers in the cross-reference are correct, and refer to the
   --      original line number, but there is no information that allows a
   --      reader of the ALI file to determine the exact mapping of physical
   --      line numbers back to the original source.

   --      Files with a zero checksum and a non-zero time stamp are in general
   --      files on which the compilation depends but which are not Ada files
   --      with further dependencies. This includes preprocessor data files
   --      and preprocessor definition files.

   --      Note: blank lines are ignored when the library information is read,
   --      and separate sections of the file are separated by blank lines to
   --      ease readability. Blanks between fields are also ignored.

   --      For entries corresponding to files that were not present (and thus
   --      resulted in error messages), or for files that are not part of the
   --      dependency set, both the time stamp and checksum are set to all zero
   --      characters. These dummy entries are ignored by the binder in
   --      dependency checking, but must be present for proper interpretation
   --      of the cross-reference data.

   --------------------------
   -- Cross-Reference Data --
   --------------------------

   --  The cross-reference data follows the dependency lines. See the spec of
   --  Lib.Xref in file lib-xref.ads for details on the format of this data.

   ---------------------------------
   -- Source Coverage Obligations --
   ---------------------------------

   --  The Source Coverage Obligation (SCO) information follows the cross-
   --  reference data. See the spec of Par_SCO in file par_sco.ads for full
   --  details of the format.

   ---------------------------------------
   -- SPARK Cross-Reference Information --
   ---------------------------------------

   --  The SPARK cross-reference information follows the SCO information. See
   --  the spec of SPARK_Xrefs in file spark_xrefs.ads for full details of the
   --  format.

   ----------------------
   -- Global Variables --
   ----------------------

   --  The table defined here stores one entry for each Interrupt_State pragma
   --  encountered either in the main source or in an ancillary with'ed source.
   --  Since interrupt state values have to be consistent across all units in a
   --  partition, we detect inconsistencies at compile time when we can.

   type Interrupt_State_Entry is record
      Interrupt_Number : Pos;
      --  Interrupt number value

      Interrupt_State : Character;
      --  Set to r/s/u for Runtime/System/User

      Pragma_Loc : Source_Ptr;
      --  Location of pragma setting this value in place
   end record;

   package Interrupt_States is new Table.Table (
     Table_Component_Type => Interrupt_State_Entry,
     Table_Index_Type     => Nat,
     Table_Low_Bound      => 1,
     Table_Initial        => 30,
     Table_Increment      => 200,
     Table_Name           => "Name_Interrupt_States");

   --  The table structure defined here stores one entry for each
   --  Priority_Specific_Dispatching pragma encountered either in the main
   --  source or in an ancillary with'ed source. Since have to be consistent
   --  across all units in a partition, we may as well detect inconsistencies
   --  at compile time when we can.

   type Specific_Dispatching_Entry is record
      Dispatching_Policy : Character;
      --  First character (upper case) of the corresponding policy name

      First_Priority     : Nat;
      --  Lower bound of the priority range to which the specified dispatching
      --  policy applies.

      Last_Priority      : Nat;
      --  Upper bound of the priority range to which the specified dispatching
      --  policy applies.

      Pragma_Loc         : Source_Ptr;
      --  Location of pragma setting this value in place
   end record;

   package Specific_Dispatching is new Table.Table (
     Table_Component_Type => Specific_Dispatching_Entry,
     Table_Index_Type     => Nat,
     Table_Low_Bound      => 1,
     Table_Initial        => 10,
     Table_Increment      => 100,
     Table_Name           => "Name_Priority_Specific_Dispatching");

   -----------------
   -- Subprograms --
   -----------------

   procedure Ensure_System_Dependency;
   --  This procedure ensures that a dependency is created on system.ads. Even
   --  if there is no semantic dependency, Targparm has read the file to
   --  acquire target parameters, so we need a source dependency.

   procedure Write_ALI (Object : Boolean);
   --  This procedure writes the library information for the current main unit
   --  The Object parameter is true if an object file is created, and false
   --  otherwise.
   --
   --  Note: in the case where we are not generating code (-gnatc mode), this
   --  routine only writes an ALI file if it cannot find an existing up to
   --  date ALI file. If it *can* find an existing up to date ALI file, then
   --  it reads this file and sets the Lib.Compilation_Arguments table from
   --  the A lines in this file.

   procedure Add_Preprocessing_Dependency (S : Source_File_Index);
   --  Indicate that there is a dependency to be added on a preprocessing data
   --  file or on a preprocessing definition file.

end Lib.Writ;
