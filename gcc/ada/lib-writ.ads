------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             L I B . W R I T                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.3 $
--                                                                          --
--          Copyright (C) 1992-2001 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
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

   --  Note: the contents of the ali file are summarized in the GNAT
   --  user's guide, so if any non-trivial changes are made to this
   --  section, they should be reflected in the user's guide.

   --  This section  describes the format of the library information that is
   --  associated with object files. The exact method of this association is
   --  potentially implementation dependent and is described and implemented
   --  in package From the point of view of the description here, all we
   --  need to know is that the information is represented as a string of
   --  characters that is somehow associated with an object file, and can be
   --  retrieved. If no library information exists for a given object file,
   --  then we take this as equivalent to the non-existence of the object
   --  file, as if source file has not been previously compiled.

   --  The library information is written as a series of lines of the form:

   --    Key_Character parameter parameter ...

   ------------------
   -- Header Lines --
   ------------------

   --  The initial header lines in the file give information about the
   --  compilation environment, and identify other special information
   --  such as main program parameters.

   --  ----------------
   --  -- V  Version --
   --  ----------------

   --    V "xxxxxxxxxxxxxxxx"
   --
   --      This line indicates the library output version, as defined in
   --      Gnatvsn. It ensures that separate object modules of a program are
   --      consistent. It has to be changed if anything changes which would
   --      affect successful binding of separately compiled modules.
   --      Examples of such changes are modifications in the format of the
   --      library info described in this package, or modifications to
   --      calling sequences, or to the way that data is represented.

   --  ---------------------
   --  -- M  Main Program --
   --  ---------------------

   --    M type [priority] [T=time-slice] W=?

   --      This line appears only if the main unit for this file is
   --      suitable for use as a main program. The parameters are:

   --        type

   --          P for a parameterless procedure
   --          F for a function returning a value of integral type
   --            (used for writing a main program returning an exit status)

   --        priority

   --          Present only if there was a valid pragma Priority in the
   --          corresponding unit to set the main task priority. It is
   --          an unsigned decimal integer.

   --        T=time-slice

   --          Present only if there was a valid pragma Time_Slice in the
   --          corresponding unit. It is an unsigned decimal integer in
   --          the range 0 .. 10**9 giving the time slice value in units
   --          of milliseconds. The actual significance of this parameter
   --          is target dependent.

   --        W=?

   --          This parameter indicates the wide character encoding
   --          method used when compiling the main program file. The ?
   --          character is the single character used in the -gnatW?
   --          switch. This is used to provide the default wide-character
   --          encoding for Wide_Text_IO files.

   --  -----------------
   --  -- A  Argument --
   --  -----------------

   --    A argument

   --      One of these lines appears for each of the arguments present
   --      in the call to the gnat1 program. This can be used if it is
   --      necessary to reconstruct this call (e.g. for fix and continue)

   --  -------------------
   --  -- P  Parameters --
   --  -------------------

   --    P <<parameters>>

   --      Indicates various information that applies to the compilation
   --      of the corresponding source unit. Parameters is a sequence of
   --      zero or more two letter codes that indicate configuration
   --      pragmas and other parameters that apply:
   --
   --      Present if the unit uses tasking directly or indirectly and
   --      has one or more valid xxx_Policy pragmas that apply to the unit.
   --      The arguments are as follows:
   --
   --         CE   Compilation errors. If this is present it means that the
   --              ali file resulted from a compilation with the -gnatQ
   --              switch set, and illegalities were detected. The ali
   --              file contents may not be completely reliable, but the
   --              format will be correct and complete. Note that NO is
   --              always present if CE is present.
   --
   --         FD   Configuration pragmas apply to all the units in this
   --              file specifying a possibly non-standard floating point
   --              format (VAX float with Long_Float using D_Float)
   --
   --         FG   Configuration pragmas apply to all the units in this
   --              file specifying a possibly non-standard floating point
   --              format (VAX float with Long_Float using G_Float)
   --
   --         FI   Configuration pragmas apply to all the units in this
   --              file specifying a possibly non-standard floating point
   --              format (IEEE Float)
   --
   --         Lx   A valid Locking_Policy pragma applies to all the units
   --              in this file, where x is the first character (upper case)
   --              of the policy name (e.g. 'C' for Ceiling_Locking)
   --
   --         NO   No object. This flag indicates that the units in this
   --              file were not compiled to produce an object. This can
   --              occur as a result of the use of -gnatc, or if no object
   --              can be produced (e.g. when a package spec is compiled
   --              instead of the body, or a subunit on its own).
   --
   --         NR   No_Run_Time pragma in effect for all units in this file
   --
   --         NS   Normalize_Scalars pragma in effect for all units in
   --              this file
   --
   --         Qx   A valid Queueing_Policy pragma applies to all the units
   --              in this file, where x is the first character (upper case)
   --              of the policy name (e.g. 'P' for Priority_Queueing).
   --
   --         Tx   A valid Task_Dispatching_Policy pragma applies to all
   --              the units in this file, where x is the first character
   --              (upper case) of the corresponding policy name (e.g. 'F'
   --              for FIFO_Within_Priorities).
   --
   --         UA  Unreserve_All_Interrupts pragma was processed in one or
   --             more units in this file
   --
   --         UX  Generated code contains unit exception table pointer
   --             (i.e. it uses zero-cost exceptions, and there is at
   --             least one subprogram present).
   --
   --         ZX  Units in this file use zero-cost exceptions and have
   --             generated exception tables. If ZX is not present, the
   --             longjmp/setjmp exception scheme is in use.
   --
   --      Note that language defined units never output policy (Lx,Tx,Qx)
   --      parameters. Language defined units must correctly handle all
   --      possible cases. These values are checked for consistency by the
   --      binder and then copied to the generated binder output file.

   --  ---------------------
   --  -- R  Restrictions --
   --  ---------------------

   --    R <<restriction-characters>>

   --      This line records information regarding restrictions. The
   --      parameter is a string of characters, one for each entry in
   --      Restrict.Partition_Restrictions, in order. There are three
   --      settings possible settings for each restriction:

   --        r   Restricted. Unit was compiled under control of a pragma
   --            Restrictions for the corresponding restriction. In
   --            this case the unit certainly does not violate the
   --            Restriction, since this would have been detected by
   --            the compiler.

   --        n   Not used. The unit was not compiled under control of a
   --            pragma Restrictions for the corresponding restriction,
   --            and does not make any use of the referenced feature.

   --        v   Violated. The unit was not compiled uner control of a
   --            pragma Restrictions for the corresponding restriction,
   --            and it does indeed use the referenced feature.

   --      This information is used in the binder to check consistency,
   --      i.e. to detect cases where one unit has "r" and another unit
   --      has "v", which is not permitted, since these restrictions
   --      are partition-wide.

   ----------------------------
   -- Compilation Unit Lines --
   ----------------------------

   --  Following these header lines, a set of information lines appears for
   --  each compilation unit that appears in the corresponding object file.
   --  In particular, when a package body or subprogram body is compiled,
   --  there will be two sets of information, one for the spec and one for
   --  the body. with the entry for the body appearing first. This is the
   --  only case in which a single ALI file contains more than one unit (in
   --  particular note that subunits do *not* count as compilation units for
   --  this purpose, and generate no library information, since they are
   --  inlined).

   --  --------------------
   --  -- U  Unit Header --
   --  --------------------

   --  The lines for each compilation unit have the following form.

   --    U unit-name source-name version <<attributes>>
   --
   --      This line identifies the unit to which this section of the
   --      library information file applies. The first three parameters are
   --      the unit name in internal format, as described in package Uname,
   --      and the name of the source file containing the unit.
   --
   --      Version is the version given as eight hexadecimal characters
   --      with upper case letters. This value is the exclusive or of the
   --      source checksums of the unit and all its semantically dependent
   --      units.
   --
   --      The <<attributes>> are a series of two letter codes indicating
   --      information about the unit:
   --
   --         DE  Dynamic Elaboration. This unit was compiled with the
   --             dynamic elaboration model, as set by either the -gnatE
   --             switch or pragma Elaboration_Checks (Dynamic).
   --
   --         EB  Unit has pragma Elaborate_Body
   --
   --         EE  Elaboration entity is present which must be set true when
   --             the unit is elaborated. The name of the elaboration entity
   --             is formed from the unit name in the usual way. If EE is
   --             present, then this boolean must be set True as part of the
   --             elaboration processing routine generated by the binder.
   --             Note that EE can be set even if NE is set. This happens
   --             when the boolean is needed solely for checking for the
   --             case of access before elaboration.
   --
   --         GE  Unit is a generic declaration, or corresponding body
   --
   --         IL  Unit source uses a style with identifiers in all lower
   --         IU  case (IL) or all upper case (IU). If the standard mixed-
   --             case usage is detected, or the compiler cannot determine
   --             the style, then no I parameter will appear.
   --
   --         IS  Initialize_Scalars pragma applies to this unit
   --
   --         KM  Unit source uses a style with keywords in mixed case
   --         KU  (KM) or all upper case (KU). If the standard lower-case
   --             usage is detected, or the compiler cannot determine the
   --             style, then no K parameter will appear.
   --
   --         NE  Unit has no elaboration routine. All subprogram bodies
   --             and specs are in this category. Package bodies and specs
   --             may or may not have NE set, depending on whether or not
   --             elaboration code is required. Set if N_Compilation_Unit
   --             node has flag Has_No_Elaboration_Code set.
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

   --  ---------------------
   --  -- W  Withed Units --
   --  ---------------------

   --  Following each U line, is a series of lines of the form

   --    W unit-name [source-name lib-name] [E] [EA] [ED]
   --
   --      One of these lines is present for each unit that is mentioned in
   --      an explicit with clause by the current unit. The first parameter
   --      is the unit name in internal format. The second parameter is the
   --      file name of the file that must be compiled to compile this unit
   --      (which is usually the file for the body, except for packages
   --      which have no body). The third parameter is the file name of the
   --      library information file that contains the results of compiling
   --      this unit. The optional modifiers are used as follows:
   --
   --        E   pragma Elaborate applies to this unit
   --
   --        EA  pragma Elaborate_All applies to this unit
   --
   --        ED  Elaborate_All_Desirable set for this unit, which means
   --            that there is no Elaborate_All, but the analysis suggests
   --            that Program_Error may be raised if the Elaborate_All
   --            conditions cannot be satisfied. The binder will attempt
   --            to treat ED as EA if it can.
   --
   --      The parameter source-name and lib-name are omitted for the case
   --      of a generic unit compiled with earlier versions of GNAT which
   --      did not generate object or ali files for generics.

   ---------------------
   -- Reference Lines --
   ---------------------

   --  The reference lines contain information about references from
   --  any of the units in the compilation (including, body version
   --  and version attributes, linker options pragmas and source
   --  dependencies.

   --  -----------------------
   --  -- L  Linker_Options --
   --  -----------------------

   --  Following the unit information is an optional series of lines that
   --  indicates the usage of pragma Linker_Options. For each appearence
   --  of pragma Linker_Actions in any of the units for which unit lines
   --  are present, a line of the form:

   --    L "string"

   --      where string is the string from the unit line enclosed in quotes.
   --      Within the quotes the following can occur:

   --        c    graphic characters in range 20-7E other than " or {
   --        ""   indicating a single " character
   --        {hh} indicating a character whose code is hex hh (0-9,A-F)
   --        {00} [ASCII.NUL] is used as a separator character
   --             to separate multiple arguments of a single
   --             Linker_Options pragma.

   --      For further details, see Stringt.Write_String_Table_Entry. Note
   --      that wide characters in the form {hhhh} cannot be produced, since
   --      pragma Linker_Option accepts only String, not Wide_String.

   --  ------------------------------------
   --  -- E  External Version References --
   --  ------------------------------------

   --  One of these lines is present for each use of 'Body_Version or
   --  'Version in any of the units of the compilation. These are used
   --  by the linker to determine which version symbols must be output.
   --  The format is simply:

   --    E name

   --  where name is the external name, i.e. the unit name with either
   --  a S or a B for spec or body version referenced (Body_Version
   --  always references the body, Version references the Spec, except
   --  in the case of a reference to a subprogram with no separate spec).
   --  Upper half and wide character codes are encoded using the same
   --  method as in Namet (Uhh for upper half, Whhhh for wide character,
   --  where hh are hex digits).

   --  ---------------------
   --  -- D  Dependencies --
   --  ---------------------

   --  The dependency lines indicate the source files on which the compiled
   --  units depend. This is used by the binder for consistency checking.
   --  These lines are also referenced by the cross-reference information.

   --    D source-name time-stamp checksum [subunit-name] line:file-name

   --      The time-stamp field contains the time stamp of the
   --      corresponding source file. See types.ads for details on
   --      time stamp representation.

   --      The checksum is an 8-hex digit representation of the source
   --      file checksum, with letters given in lower case.

   --      The subunit name is present only if the dependency line is for
   --      a subunit. It contains the fully qualified name of the subunit
   --      in all lower case letters.

   --      The line:file-name entry is present only if a Source_Reference
   --      pragma appeared in the source file identified by source-name.
   --      In this case, it gives the information from this pragma. Note
   --      that this allows cross-reference information to be related back
   --      to the original file. Note: the reason the line number comes
   --      first is that a leading digit immediately identifies this as
   --      a Source_Reference entry, rather than a subunit-name.

   --      A line number of zero for line: in this entry indicates that
   --      there is more than one source reference pragma. In this case,
   --      the line numbers in the cross-reference are correct, and refer
   --      to the original line number, but there is no information that
   --      allows a reader of the ALI file to determine the exact mapping
   --      of physical line numbers back to the original source.

   --      Note: blank lines are ignored when the library information is
   --      read, and separate sections of the file are separated by blank
   --      lines to ease readability. Blanks between fields are also
   --      ignored.

   --      For entries corresponding to files that were not present (and
   --      thus resulted in error messages), or for files that are not
   --      part of the dependency set, both the time stamp and checksum
   --      are set to all zero characters. These dummy entries are ignored
   --      by the binder in dependency checking, but must be present for
   --      proper interpretation of the cross-reference data.

   --------------------------
   -- Cross-Reference Data --
   --------------------------

   --  The cross-reference data follows the dependency lines. See
   --  the spec of Lib.Xref for details on the format of this data.

   -----------------
   -- Subprograms --
   -----------------

   procedure Ensure_System_Dependency;
   --  This procedure ensures that a dependency is created on system.ads.
   --  Even if there is no semantic dependency, Targparm has read the
   --  file to acquire target parameters, so we need a source dependency.

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

end Lib.Writ;
