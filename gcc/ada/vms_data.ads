------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             V M S _ D A T A                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1996-2011, Free Software Foundation, Inc.         --
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

--  This package contains, for each of the command of the GNAT driver, one
--  constant array; each component of this array is a string that defines,
--  in coded form as explained below, the conversion of a VMS qualifier of the
--  command to the corresponding switch of the GNAT tool corresponding to the
--  command.

--  This package is used by the GNAT driver to invokes the GNAT tools with the
--  switches corresponding to the VMS qualifier and by the Project Manager to
--  convert VMS qualifiers in project files to their corresponding switch
--  values.

--  This package is also an input to the tool that generates the VMS GNAT
--  help information automatically.

--  NOTE: the format of this package must follow the following rules, so that
--        the VMS GNAT help tool works properly:

--    - Each command zone (where the eventual qualifiers are declared) must
--      begin with a boxed comment of the form:

--      ---------------------------------
--      -- Switches for GNAT <COMMAND> --
--      ---------------------------------

--      where <COMMAND> is the name of a GNAT command in capital letters, for
--      example BIND, COMPILE, XREF, ...

--    - each qualifier declaration must be followed either by
--         - a comment starting with "--  NODOC", to indicate that there is
--           no documentation for this qualifier, or
--         - a contiguous sequence of comments that constitute the
--           documentation of the qualifier.

--    - each command zone ends with the declaration of the constant array
--      for the command, of the form:

--      <Command>__Switches : aliased constant Switches :=

package VMS_Data is

   ----------------
   -- QUALIFIERS --
   ----------------

   --  The syntax of a qualifier declaration is as follows:

   --    SWITCH_STRING ::= "/ command-qualifier-name TRANSLATION"

   --    TRANSLATION ::=
   --      DIRECT_TRANSLATION
   --    | DIRECTORIES_TRANSLATION
   --    | FILE_TRANSLATION
   --    | NO_SPACE_FILE_TRANSL
   --    | NUMERIC_TRANSLATION
   --    | STRING_TRANSLATION
   --    | OPTIONS_TRANSLATION
   --    | COMMANDS_TRANSLATION
   --    | ALPHANUMPLUS_TRANSLATION
   --    | OTHER_TRANSLATION

   --    DIRECT_TRANSLATION       ::= space UNIX_SWITCHES
   --    DIRECTORIES_TRANSLATION  ::= =* UNIX_SWITCH *
   --    DIRECTORY_TRANSLATION    ::= =% UNIX_SWITCH %
   --    FILE_TRANSLATION         ::= =@ UNIX_SWITCH @
   --    NO_SPACE_FILE_TRANSL     ::= =< UNIX_SWITCH >
   --    NUMERIC_TRANSLATION      ::= =# UNIX_SWITCH # | # number #
   --    STRING_TRANSLATION       ::= =" UNIX_SWITCH "
   --    OPTIONS_TRANSLATION      ::= =OPTION {space OPTION}
   --    COMMANDS_TRANSLATION     ::= =? ARGS space command-name
   --    ALPHANUMPLUS_TRANSLATION ::= =| UNIX_SWITCH |

   --    UNIX_SWITCHES ::= UNIX_SWITCH {, UNIX_SWITCH}

   --    UNIX_SWITCH ::= unix-switch-string | !unix-switch-string | `string'

   --    OPTION ::= option-name space UNIX_SWITCHES

   --    ARGS ::= -cargs | -bargs | -largs

   --  Here command-qual is the name of the switch recognized by the GNATCmd.
   --  This is always given in upper case in the templates, although in the
   --  actual commands, either upper or lower case is allowed.

   --  The unix-switch-string always starts with a minus, and has no commas
   --  or spaces in it. Case is significant in the unix switch string. If a
   --  unix switch string is preceded by the not sign (!) it means that the
   --  effect of the corresponding command qualifier is to remove any previous
   --  occurrence of the given switch in the command line.

   --  The DIRECTORIES_TRANSLATION format is used where a list of directories
   --  is given. This possible corresponding formats recognized by GNATCmd are
   --  as shown by the following example for the case of PATH

   --    PATH=direc
   --    PATH=(direc,direc,direc,direc)

   --  When more than one directory is present for the DIRECTORIES case, then
   --  multiple instances of the corresponding unix switch are generated,
   --  with the file name being substituted for the occurrence of *.

   --  The FILE_TRANSLATION format is similar except that only a single
   --  file is allowed, not a list of files, and only one unix switch is
   --  generated as a result.

   --  the NO_SPACE_FILE_TRANSL is similar to FILE_TRANSLATION, except that
   --  no space is inserted between the switch and the file name.

   --  The NUMERIC_TRANSLATION format is similar to the FILE_TRANSLATION case
   --  except that the parameter is a decimal integer in the range 0 to 999999.

   --  For the OPTIONS_TRANSLATION case, GNATCmd similarly permits one or
   --  more options to appear (although only in some cases does the use of
   --  multiple options make logical sense). For example, taking the
   --  case of ERRORS for GCC, the following are all allowed:

   --    /ERRORS=BRIEF
   --    /ERRORS=(FULL,VERBOSE)
   --    /ERRORS=(BRIEF IMMEDIATE)

   --  If no option is provided (e.g. just /ERRORS is written), then the
   --  first option in the list is the default option. For /ERRORS this
   --  is NORMAL, so /ERRORS with no option is equivalent to /ERRORS=NORMAL.

   --  The COMMANDS_TRANSLATION case is only used for gnatmake, to correspond
   --  to the use of -cargs, -bargs and -largs (the ARGS string as indicated
   --  is one of these three possibilities). The name given by COMMAND is the
   --  corresponding command name to be used to interpret the switches to be
   --  passed on. Switches of this type set modes, e.g. /COMPILER_QUALIFIERS
   --  sets the mode so that all subsequent switches, up to another switch
   --  with COMMANDS_TRANSLATION apply to the corresponding commands issued
   --  by the make utility. For example

   --    /COMPILER_QUALIFIERS /LIST /BINDER_QUALIFIERS /MAIN
   --    /COMPILER_QUALIFIERS /NOLIST /COMPILE_CHECKS=SYNTAX

   --  Clearly these switches must come at the end of the list of switches
   --  since all subsequent switches apply to an issued command.

   --  For the DIRECT_TRANSLATION case, an implicit additional qualifier
   --  declaration is created by prepending NO to the name of the qualifier,
   --  and then inverting the sense of the UNIX_SWITCHES string. For example,
   --  given the qualifier definition:

   --     "/LIST -gnatl"

   --  An implicit qualifier definition is created:

   --     "/NOLIST !-gnatl"

   --  In the case where, a ! is already present, inverting the sense of the
   --  switch means removing it.

   subtype S is String;
   --  A synonym to shorten the table

   type String_Ptr is access constant String;
   --  String pointer type used throughout

   type Switches is array (Natural range <>) of String_Ptr;
   --  Type used for array of switches

   type Switches_Ptr is access constant Switches;

   ----------------------------
   -- Switches for GNAT BIND --
   ----------------------------

   S_Bind_Add     : aliased constant S := "/ADD_PROJECT_SEARCH_DIR=*"      &
                                            "-aP*";
   --        /ADD_PROJECT_SEARCH_PATH=(directory[,...])
   --
   --   Add directories to the project search path.

   S_Bind_ALI     : aliased constant S := "/ALI_LIST "                     &
                                            "-A";
   --        /NOALI_LIST (D)
   --        /ALI_LIST
   --
   --    Output full names of all the ALI files in the partition. The output is
   --    written to SYS$OUTPUT.

   S_Bind_Bind    : aliased constant S := "/BIND_FILE="                    &
                                            "ADA "                         &
                                               "-A "                       &
                                            "C "                           &
                                               "-C";
   --        /BIND_FILE[=bind-file-option]
   --
   --   Specifies the language of the binder generated file.
   --
   --        ADA (D)    Binder file is Ada.
   --
   --        C          Binder file is 'C'.

   S_Bind_Build   : aliased constant S := "/BUILD_LIBRARY=|"               &
                                            "-L|";
   --        /BUILD_LIBRARY=xxx
   --
   --        Binds the units for library building. In this case the adainit and
   --        adafinal procedures are rename to xxxinit and xxxfinal. Implies
   --        /NOMAIN.

   S_Bind_Current : aliased constant S := "/CURRENT_DIRECTORY "            &
                                            "!-I-";
   --        /CURRENT_DIRECTORY (D)
   --        /NOCURRENT_DIRECTORY
   --
   --        Look for source, library or object files in the default directory.

   S_Bind_Debug   : aliased constant S := "/DEBUG="                        &
                                            "TRACEBACK "                   &
                                               "-g2 "                      &
                                            "ALL "                         &
                                               "-g3 "                      &
                                            "NONE "                        &
                                               "-g0 "                      &
                                            "SYMBOLS "                     &
                                               "-g1 "                      &
                                            "NOSYMBOLS "                   &
                                               "!-g1 "                     &
                                            "LINK "                        &
                                               "-g3 "                      &
                                            "NOTRACEBACK "                 &
                                               "!-g2";
   --        /DEBUG[=debug-level]
   --        /NODEBUG
   --
   --    Specify level of debugging information generated for the elaboration
   --    routine.  See corresponding qualifier for GNAT COMPILE.

   S_Bind_DebugX  : aliased constant S := "/NODEBUG "                      &
                                            "!-g";
   --  NODOC  (see /DEBUG)

   S_Bind_Elab    : aliased constant S := "/ELABORATION_DEPENDENCIES "     &
                                            "-e";
   --        /ELABORATION_DEPENDENCIES
   --        /NOELABORATION_DEPENDENCIES (D)
   --
   --   Output complete list of elaboration-order dependencies, showing the
   --   reason for each dependency. This output can be rather extensive but may
   --   be useful in diagnosing problems with elaboration order. The output is
   --   written to SYS$OUTPUT.

   S_Bind_Error   : aliased constant S := "/ERROR_LIMIT=#"                 &
                                            "-m#";
   --        /ERROR_LIMIT=nnn
   --
   --   Limit number of detected errors to nnn (1-999999).

   S_Bind_Ext     : aliased constant S := "/EXTERNAL_REFERENCE=" & '"'     &
                                            "-X" & '"';
   --       /EXTERNAL_REFERENCE="name=val"
   --
   --   Specifies an external reference to the project manager. Useful only if
   --   /PROJECT_FILE is used.
   --
   --   Example:
   --      /EXTERNAL_REFERENCE="DEBUG=TRUE"

   S_Bind_Follow  : aliased constant S := "/FOLLOW_LINKS_FOR_FILES "       &
                                            "-eL";
   --        /NOFOLLOW_LINKS_FOR_FILES (D)
   --        /FOLLOW_LINKS_FOR_FILES
   --
   --    Follow links when parsing project files

   S_Bind_Force   : aliased constant S := "/FORCE_ELAB_FLAGS "             &
                                            "-F";
   --        /NOFORCE_ELAB_FLAGS (D)
   --        /FORCE_ELAB_FLAGS
   --
   --    Force checking of elaboration Flags

   S_Bind_Help    : aliased constant S := "/HELP "                         &
                                            "-h";
   --        /HELP
   --
   --   Output usage information.

   S_Bind_Init    : aliased constant S := "/INITIALIZE_SCALARS="           &
                                            "INVALID "                     &
                                               "-Sin "                     &
                                            "LOW "                         &
                                               "-Slo "                     &
                                            "HIGH "                        &
                                               "-Shi";
   --        /INITIALIZE_SCALARS[=scalar-option]
   --
   --   Indicate how uninitialized scalar values for which a pragma
   --   Initialize_Scalars applies should be initialized.
   --   scalar-option may be one of the following:
   --
   --      INVALID (D)  Initialize with an invalid value.
   --      LOW          Initialize with the lowest valid value of the subtype.
   --      HIGH         Initialize with the highest valid value of the subtype.

   S_Bind_Leap    : aliased constant S := "/ENABLE_LEAP_SECONDS "          &
                                            "-y";
   --      /ENABLE_LEAP_SECONDS
   --      /NOENABLE_LEAP_SECONDS (D)
   --
   --   Enable leap seconds support in Ada.Calendar and its children.

   S_Bind_Library : aliased constant S := "/LIBRARY_SEARCH=*"              &
                                            "-aO*";
   --        /LIBRARY_SEARCH=(direc[,...])
   --
   --        When looking for library and object files look also in directories
   --        specified.

   S_Bind_Linker  : aliased constant S := "/LINKER_OPTION_LIST "           &
                                            "-K";
   --        /NOLINKER_OPTION_LIST (D)
   --        /LINKER_OPTION_LIST
   --
   --        Output linker options to SYS$OUTPUT.  Includes library search
   --        paths, contents of pragmas Ident and Linker_Options, and
   --        libraries added by GNAT BIND.

   S_Bind_Main    : aliased constant S := "/MAIN "                         &
                                            "!-n";
   --        /MAIN (D)
   --
   --   The main program is in Ada.
   --
   --        /NOMAIN
   --
   --   The main program is not in Ada.

   S_Bind_Alloc32 : aliased constant S := "/32_MALLOC "                    &
                                            "-H32";
   --        /32_MALLOC
   --
   --        Use 32-bit allocations for `__gnat_malloc' (and thus for
   --        access types).

   S_Bind_Alloc64 : aliased constant S := "/64_MALLOC "                    &
                                            "-H64";
   --        /64_MALLOC
   --
   --        Use 64-bit allocations for `__gnat_malloc' (and thus for
   --        access types).

   S_Bind_Mess    : aliased constant S := "/MESSAGES_PROJECT_FILE="        &
                                            "DEFAULT "                     &
                                               "-vP0 "                     &
                                            "MEDIUM "                      &
                                               "-vP1 "                     &
                                            "HIGH "                        &
                                               "-vP2";
   --        /MESSAGES_PROJECT_FILE[=messages-option]
   --
   --   Specifies the "verbosity" of the parsing of project files.
   --   messages-option may be one of the following:
   --
   --      DEFAULT (D)  No messages are output if there is no error or warning.
   --
   --      MEDIUM       A small number of messages are output.
   --
   --      HIGH         A great number of messages are output, most of them not
   --                   being useful for the user.

   S_Bind_Nostinc : aliased constant S := "/NOSTD_INCLUDES "               &
                                            "-nostdinc";
   --        /NOSTD_INCLUDES
   --
   --    Do not look for sources the in the system default directory.

   S_Bind_Nostlib : aliased constant S := "/NOSTD_LIBRARIES "              &
                                            "-nostdlib";
   --        /NOSTD_LIBRARIES
   --
   --    Do not look for library files in the system default directory.

   S_Bind_No_Time : aliased constant S := "/NO_TIME_STAMP_CHECK "          &
                                            "-t";
   --  NODOC (see /TIME_STAMP_CHECK)

   S_Bind_Object  : aliased constant S := "/OBJECT_LIST "                  &
                                            "-O";
   --        /NOOBJECT_LIST (D)
   --        /OBJECT_LIST
   --
   --    Output full names of all the object files that must be linked to
   --    provide the Ada component of the program. The output is written to
   --    SYS$OUTPUT.

   S_Bind_Order   : aliased constant S := "/ORDER_OF_ELABORATION "         &
                                            "-l";
   --        /NOORDER_OF_ELABORATION (D)
   --        /ORDER_OF_ELABORATION
   --
   --   Output chosen elaboration order. The output is written to SYS$OUTPUT.

   S_Bind_Output  : aliased constant S := "/OUTPUT=@"                      &
                                            "-o@";
   --        /OUTPUT=filename
   --
   --   File name to use for the program containing the elaboration code.

   S_Bind_OutputX : aliased constant S := "/NOOUTPUT "                     &
                                            "-c";
   --        /NOOUTPUT
   --
   --   Check only. Do not generate the binder output file.
   --
   --   In this mode the binder performs all error checks but does not generate
   --   an output file.

   S_Bind_Pess    : aliased constant S := "/PESSIMISTIC_ELABORATION "      &
                                            "-p";
   --        /PESSIMISTIC_ELABORATION
   --
   --   Causes the binder to choose a "pessimistic" elaboration order, i.e. one
   --   which is most likely to cause elaboration order problems. This can be
   --   useful in testing portable code to make sure that there are no missing
   --   elaborate pragmas.

   S_Bind_Project : aliased constant S := "/PROJECT_FILE=<"                &
                                            "-P>";
   --        /PROJECT_FILE=filename
   --
   --   Specifies the main project file to be used. The project files rooted
   --   at the main project file will be parsed before the invocation of the
   --   binder. The source and object directories to be searched will be
   --   communicated to the binder through logical names ADA_PRJ_INCLUDE_FILE
   --   and ADA_PRJ_OBJECTS_FILE.

   S_Bind_Read    : aliased constant S := "/READ_SOURCES="                 &
                                            "ALL "                         &
                                               "-s "                       &
                                            "NONE "                        &
                                               "-x "                       &
                                            "AVAILABLE "                   &
                                               "!-x,!-s";
   --        /READ_SOURCES[=(keyword[,...])]
   --        /NOREAD_SOURCES
   --
   --   The following keyword are accepted:
   --
   --      ALL (D)      Require source files to be present. In this mode, the
   --                   binder insists on being able to locate all source files
   --                   that are referenced and checks their consistency.  In
   --                   normal mode, if a source file cannot be located it is
   --                   simply ignored. If you specify the ALL keyword, a
   --                   missing source file is an error.
   --
   --      NONE         Exclude source files. In this mode, the binder only
   --                   checks that ALI files are consistent with one another.
   --                   source files are not accessed.  The binder runs faster
   --                   in this mode, and there is still a guarantee that the
   --                   resulting program is self-consistent.
   --
   --                   If a source file has been edited since it was last
   --                   compiled and you specify the NONE keyword, the binder
   --                   will not detect that the object file is out of date
   --                   with the source file.
   --
   --                   This is the same as specifying /NOREAD_SOURCES.
   --
   --      AVAILABLE    Check that object files are consistent with one
   --                   another and are consistent with any source files that
   --                   can be located.

   S_Bind_ReadX   : aliased constant S := "/NOREAD_SOURCES "               &
                                            "-x";
   --  NODOC (see /READ_SOURCES)

   S_Bind_Rename  : aliased constant S := "/RENAME_MAIN=<"                 &
                                            "-M>";
   --        /RENAME_MAIN=xxx
   --
   --   Renames the generated main program from main to xxx.
   --   This is useful in the case of some cross-building environments, where
   --   the actual main program is separate from the one generated
   --   by GNAT BIND.

   S_Bind_Report  : aliased constant S := "/REPORT_ERRORS="                &
                                            "VERBOSE "                     &
                                               "-v "                       &
                                            "BRIEF "                       &
                                               "-b "                       &
                                            "DEFAULT "                     &
                                               "!-b,!-v";
   --        /REPORT_ERRORS[=(keyword[,...])]
   --           VERBOSE (D)
   --           BRIEF
   --           DEFAULT
   --        /NOREPORT_ERRORS
   --
   --   With the DEFAULT keyword (which is not the default when the binder is
   --   run from GNAT BIND) or the /NOREPORT_ERRORS qualifier, brief error
   --   messages are generated to SYS$ERROR.  If the VERBOSE keyword is
   --   present, a header is written to SYS$OUTPUT and any error messages are
   --   directed to SYS$OUTPUT  All that is written to SYS$ERROR is a brief
   --   summary message.
   --
   --   If the BRIEF keyword is specified, the binder will generate brief error
   --   messages to SYS$ERROR even if verbose mode is specified. This is
   --   relevant only when used together with the VERBOSE keyword or /VERBOSE
   --   qualifier.

   S_Bind_ReportX : aliased constant S := "/NOREPORT_ERRORS "              &
                                            "!-b,!-v";
   --  NODOC (see /REPORT_ERRORS)

   S_Bind_Restr   : aliased constant S := "/RESTRICTION_LIST "             &
                                            "-r";
   --        /NORESTRICTION_LIST (D)
   --        /RESTRICTION_LIST
   --
   --   Generate list of pragma Restrictions that could be applied to the
   --   current unit. This is useful for code audit purposes, and also may be
   --   used to improve code generation in some cases.

   S_Bind_Return  : aliased constant S := "/RETURN_CODES="                 &
                                            "POSIX "                       &
                                               "!-X1 "                     &
                                            "VMS "                         &
                                               "-X1";
   --        /RETURN_CODES=POSIX (D)
   --        /RETURN_CODES=VMS
   --
   --   Specifies the style of default exit code returned. Must be used in
   --   conjunction with and match the Link qualifier with same name.
   --
   --        POSIX (D)   Return Posix success (0) by default.
   --
   --        VMS         Return VMS success (1) by default.

   S_Bind_RTS     : aliased constant S := "/RUNTIME_SYSTEM=|"              &
                                            "--RTS=|";
   --      /RUNTIME_SYSTEM=xxx
   --
   --    Binds against an alternate runtime system named xxx or RTS-xxx.

   S_Bind_Search  : aliased constant S := "/SEARCH=*"                      &
                                            "-I*";
   --        /SEARCH=(directory[,...])
   --
   --   When looking for source or object files also look in directories
   --   specified.
   --
   --   This is the same as specifying both /LIBRARY_SEARCH and /SOURCE_SEARCH
   --   for a directory.

   S_Bind_Shared  : aliased constant S := "/SHARED "                       &
                                            "-shared,!-static";
   --        /SHARED
   --        /NOSHARED
   --
   --    Link against a shared GNAT run time when available.

   S_Bind_Slice   : aliased constant S := "/TIME_SLICE=#"                  &
                                            "-T#";
   --        /TIME_SLICE=nnn
   --
   --   Set the time slice value to nnn milliseconds. A value of zero means no
   --   time slicing and also indicates to the tasking run time to match as
   --   close as possible to the annex D requirements of the RM.

   S_Bind_Source  : aliased constant S := "/SOURCE_SEARCH=*"               &
                                            "-aI*";
   --        /SOURCE_SEARCH=(directory[,...])
   --
   --    When looking for source files also look in directories specified.

   S_Bind_Static  : aliased constant S := "/STATIC "                       &
                                            "-static,!-shared";
   --        /STATIC
   --        /NOSTATIC
   --
   --    Link against a static GNAT run time.

   S_Bind_Store   : aliased constant S := "/STORE_TRACEBACKS "             &
                                            "-E";
   --        /STORE_TRACEBACKS (D)
   --        /NOSTORE_TRACEBACKS
   --
   --   Store tracebacks in exception occurrences.
   --   This is the default on VMS, with the zero-cost exception mechanism.
   --   This qualifier has no impact, except when using the setjmp/longjmp
   --   exception mechanism, with the GNAT COMPILE qualifier /LONGJMP_SETJMP.

   S_Bind_Subdirs : aliased constant S := "/SUBDIRS=<"                     &
                                            "--subdirs=>";
   --        /SUBDIRS=dir
   --
   --   The actual directories (object, exec, library, ...) are subdirectories
   --   of the directory specified in the project file. If the subdirectory
   --   does not exist, it is created automatically.

   S_Bind_Time    : aliased constant S := "/TIME_STAMP_CHECK "             &
                                            "!-t";
   --      /TIME_STAMP_CHECK (D)
   --
   --   Time stamp errors will be treated as errors.
   --
   --      /NOTIME_STAMP_CHECK
   --
   --   Ignore time stamp errors. Any time stamp error messages are treated as
   --   warning messages. This switch essentially disconnects the normal
   --   consistency checking, and the resulting program may have undefined
   --   semantics if inconsistent units are present.
   --
   --   This means that /NOTIME_STAMP_CHECK should be used only in unusual
   --   situations, with extreme care.

   S_Bind_Verbose : aliased constant S := "/VERBOSE "                      &
                                            "-v";
   --        /VERBOSE (D)
   --        /NOVERBOSE
   --
   --   Equivalent to /REPORT_ERRORS=VERBOSE.

   S_Bind_Warn    : aliased constant S := "/WARNINGS="                     &
                                            "NORMAL "                      &
                                               "!-ws,!-we "                &
                                            "SUPPRESS "                    &
                                               "-ws "                      &
                                            "ERROR "                       &
                                               "-we";
   --      /WARNINGS[=(keyword[,...])]
   --      /NOWARNINGS
   --
   --   The following keywords are supported:
   --
   --        NORMAL (D)    Print warning messages and treat them as warning.
   --        SUPPRESS      Suppress all warning messages (same as /NOWARNINGS).
   --        ERROR         Treat any warning messages as fatal errors

   S_Bind_WarnX   : aliased constant S := "/NOWARNINGS "                   &
                                            "-ws";
   --  NODOC (see /WARNINGS)

   S_Bind_Wide    : aliased constant S := "/WIDE_CHARACTER_ENCODING="      &
                                            "BRACKETS "                    &
                                               "-gnatWb "                  &
                                            "HEX "                         &
                                               "-gnatWh "                  &
                                            "UPPER "                       &
                                               "-gnatWu "                  &
                                            "SHIFT_JIS "                   &
                                               "-gnatWs "                  &
                                            "UTF8 "                        &
                                               "-gnatW8 "                  &
                                            "EUC "                         &
                                               "-gnatWe";
   --        /NOWIDE_CHARACTER_ENCODING (D)
   --        /WIDE_CHARACTER_ENCODING[=encode-type]
   --
   --   Specifies the mechanism used to encode wide characters, overriding
   --   the default as set by the /WIDE_CHARACTER_ENCODING option for the
   --   compilation of the main program.

   S_Bind_Zero    : aliased constant S := "/ZERO_MAIN "                    &
                                            "-z";
   --        /NOZERO_MAIN (D)
   --        /ZERO_MAIN
   --
   --   Normally the binder checks that the unit name given on the command line
   --   corresponds to a suitable main subprogram. When /ZERO_MAIN is used,
   --   a list of ALI files can be given, and the execution of the program
   --   consists of elaboration of these units in an appropriate order.

   Bind_Switches : aliased constant Switches :=
                     (S_Bind_Add     'Access,
                      S_Bind_ALI     'Access,
                      S_Bind_Bind    'Access,
                      S_Bind_Build   'Access,
                      S_Bind_Current 'Access,
                      S_Bind_Debug   'Access,
                      S_Bind_DebugX  'Access,
                      S_Bind_Elab    'Access,
                      S_Bind_Error   'Access,
                      S_Bind_Ext     'Access,
                      S_Bind_Follow  'Access,
                      S_Bind_Force   'Access,
                      S_Bind_Help    'Access,
                      S_Bind_Init    'Access,
                      S_Bind_Leap    'Access,
                      S_Bind_Library 'Access,
                      S_Bind_Linker  'Access,
                      S_Bind_Main    'Access,
                      S_Bind_Alloc32 'Access,
                      S_Bind_Alloc64 'Access,
                      S_Bind_Mess    'Access,
                      S_Bind_Nostinc 'Access,
                      S_Bind_Nostlib 'Access,
                      S_Bind_No_Time 'Access,
                      S_Bind_Object  'Access,
                      S_Bind_Order   'Access,
                      S_Bind_Output  'Access,
                      S_Bind_OutputX 'Access,
                      S_Bind_Pess    'Access,
                      S_Bind_Project 'Access,
                      S_Bind_Read    'Access,
                      S_Bind_ReadX   'Access,
                      S_Bind_Rename  'Access,
                      S_Bind_Report  'Access,
                      S_Bind_ReportX 'Access,
                      S_Bind_Restr   'Access,
                      S_Bind_Return  'Access,
                      S_Bind_RTS     'Access,
                      S_Bind_Search  'Access,
                      S_Bind_Shared  'Access,
                      S_Bind_Slice   'Access,
                      S_Bind_Source  'Access,
                      S_Bind_Static  'Access,
                      S_Bind_Store   'Access,
                      S_Bind_Subdirs 'Access,
                      S_Bind_Time    'Access,
                      S_Bind_Verbose 'Access,
                      S_Bind_Warn    'Access,
                      S_Bind_WarnX   'Access,
                      S_Bind_Wide    'Access,
                      S_Bind_Zero    'Access);

   -----------------------------
   -- Switches for GNAT CHECK --
   -----------------------------

   S_Check_Add    : aliased constant S := "/ADD_PROJECT_SEARCH_DIR=*"      &
                                            "-aP*";
   --        /ADD_PROJECT_SEARCH_PATH=(directory[,...])
   --
   --   Add directories to the project search path.

   S_Check_All    : aliased constant S := "/ALL "                          &
                                            "-a";
   --        /NOALL (D)
   --        /ALL
   --
   --   Also check the components of the GNAT run time and process the needed
   --  components of the GNAT RTL when building and analyzing the global
   --  structure for checking the global rules.

   S_Check_Ext     : aliased constant S := "/EXTERNAL_REFERENCE=" & '"'    &
                                             "-X" & '"';
   --       /EXTERNAL_REFERENCE="name=val"
   --
   --   Specifies an external reference to the project manager. Useful only if
   --   /PROJECT_FILE is used.
   --
   --   Example:
   --      /EXTERNAL_REFERENCE="DEBUG=TRUE"

   S_Check_Files  : aliased constant S := "/FILES=@"                       &
                                             "-files=@";
   --      /FILES=filename
   --
   --   Take as arguments the files that are listed in the specified
   --   text file.

   S_Check_Follow : aliased constant S := "/FOLLOW_LINKS_FOR_FILES "       &
                                            "-eL";
   --        /NOFOLLOW_LINKS_FOR_FILES (D)
   --        /FOLLOW_LINKS_FOR_FILES
   --
   --    Follow links when parsing project files

   S_Check_Help   : aliased constant S := "/HELP "                         &
                                            "-h";
   --        /NOHELP (D)
   --        /HELP
   --
   --   Print information about currently implemented checks.

   S_Check_Locs   : aliased constant S := "/LOCS "                         &
                                            "-l";
   --        /NOLOCS (D)
   --        /LOCS
   --
   --   Use full source locations references in the report file.

   S_Diagnosis   : aliased constant S := "/DIAGNOSTIC_LIMIT=#"             &
                                            "-m#";
   --        /DIAGNOSTIC_LIMIT=500 (D)
   --        /DIAGNOSTIC_LIMIT=nnn
   --
   --   NNN is a decimal integer in the range of 1 to 1000 and limits the
   --   number of diagnostic messages to be generated into Stdout to that
   --   number.  Once that number has been reached, gnatcheck stops
   --   to print out diagnoses into Stderr. If NNN is equal to 0, this means
   --   that there is no limit on the number of diagnoses in Stdout.

   S_Check_Mess    : aliased constant S := "/MESSAGES_PROJECT_FILE="       &
                                             "DEFAULT "                    &
                                                "-vP0 "                    &
                                             "MEDIUM "                     &
                                                "-vP1 "                    &
                                             "HIGH "                       &
                                                "-vP2";
   --        /MESSAGES_PROJECT_FILE[=messages-option]
   --
   --   Specifies the "verbosity" of the parsing of project files.
   --   messages-option may be one of the following:
   --
   --      DEFAULT (D)  No messages are output if there is no error or warning.
   --
   --      MEDIUM       A small number of messages are output.
   --
   --      HIGH         A great number of messages are output, most of them not
   --                   being useful for the user.

   S_Check_Project : aliased constant S := "/PROJECT_FILE=<"               &
                                             "-P>";
   --        /PROJECT_FILE=filename
   --
   --   Specifies the main project file to be used. The project files rooted
   --   at the main project file will be parsed before the invocation of the
   --   gnatcheck. The source directories to be searched will be communicated
   --   to gnatcheck through logical name ADA_PRJ_INCLUDE_FILE.

   S_Check_Quiet  : aliased constant S := "/QUIET "                        &
                                            "-q";
   --        /NOQUIET (D)
   --        /QUIET
   --
   --   Work quietly, only output warnings and errors.

   S_Check_Time     : aliased constant S := "/TIME "                       &
                                               "-t";
   --        /NOTIME (D)
   --        /TIME
   --
   --   Print out execution time

   S_Check_Log      : aliased constant S := "/LOG "                        &
                                               "-log";
   --        /NOLOG (D)
   --        /LOG
   --
   --   Duplicate all the output sent to Stderr into a log file.

   S_Check_Short  : aliased constant S := "/SHORT "                        &
                                            "-s";
   --        /NOSHORT (D)
   --        /SHORT
   --
   --   Generate a short form of the report file.

   S_Check_Include : aliased constant S := "/INCLUDE_FILE=@"               &
                                            "--include-file=@";

   --        /INCLUDE_FILE=filename
   --
   --   Add the content of the specified text file to the generated report
   --   file.

   S_Check_Subdirs : aliased constant S := "/SUBDIRS=<"                    &
                                             "--subdirs=>";
   --        /SUBDIRS=dir
   --
   --   The actual directories (object, exec, library, ...) are subdirectories
   --   of the directory specified in the project file. If the subdirectory
   --   does not exist, it is created automatically.

   S_Check_Template  : aliased constant S := "/TEMPLATE=@"                 &
                                             "--write-rules=@";
   --      /TEMPLATE=filename
   --
   --   Generate the rule template into the specified file.

   S_Check_Verb   : aliased constant S := "/VERBOSE "                      &
                                            "-v";
   --        /NOVERBOSE (D)
   --        /VERBOSE
   --
   --   The version number and copyright notice are output, as well as exact
   --   copies of the gnat1 commands spawned to obtain the chop control
   --   information.

   S_Check_Out  : aliased constant S := "/OUTPUT=@"                &
                                             "-o@";
   --        /OUTPUT=filename
   --
   --   Specify the name of the output file.

   Check_Switches : aliased constant Switches :=
                      (S_Check_Add     'Access,
                       S_Check_All     'Access,
                       S_Diagnosis     'Access,
                       S_Check_Ext     'Access,
                       S_Check_Files   'Access,
                       S_Check_Follow  'Access,
                       S_Check_Help    'Access,
                       S_Check_Locs    'Access,
                       S_Check_Mess    'Access,
                       S_Check_Project 'Access,
                       S_Check_Quiet   'Access,
                       S_Check_Time    'Access,
                       S_Check_Log     'Access,
                       S_Check_Short   'Access,
                       S_Check_Include 'Access,
                       S_Check_Subdirs 'Access,
                       S_Check_Template'Access,
                       S_Check_Verb    'Access,
                       S_Check_Out     'Access);

   ----------------------------
   -- Switches for GNAT CHOP --
   ----------------------------

   S_Chop_Comp   : aliased constant S := "/COMPILATION "                   &
                                            "-c";
   --        /NOCOMPILATION (D)
   --        /COMPILATION
   --
   --   Compilation mode, handle configuration pragmas strictly according to
   --   RM rules.

   S_Chop_File   : aliased constant S := "/FILE_NAME_MAX_LENGTH=#"         &
                                            "-k#";
   --        /FILE_NAME_MAX_LENGTH[=nnn]
   --
   --   Limit generated file names to NNN (default of 8) characters. This is
   --   useful if the resulting set of files is required to be interoperable
   --   with systems like MS-DOS which limit the length of file names.

   S_Chop_Help   : aliased constant S := "/HELP "                          &
                                            "-h";
   --        /NOHELP (D)
   --        /HELP
   --
   --   Print usage information.

   S_Chop_Over   : aliased constant S := "/OVERWRITE "                     &
                                            "-w";
   --        /NOOVERWRITE (D)
   --        /OVERWRITE
   --
   --   Overwrite existing file names. Normally GNAT CHOP regards it as a
   --   fatal error situation if there is already a file with the same name as
   --   a file it would otherwise output. The /OVERWRITE qualifier bypasses
   --   this check, and any such existing files will be silently overwritten.

   S_Chop_Pres   : aliased constant S := "/PRESERVE "                      &
                                            "-p";
   --        /NOPRESERVE (D)
   --        /PRESERVE
   --
   --   Causes the file modification time stamp of the input file to be
   --   preserved and used for the time stamp of the output file(s). This may
   --   be useful for preserving coherency of time stamps in an environment
   --   where gnatchop is used as part of a standard build process.

   S_Chop_Quiet  : aliased constant S := "/QUIET "                         &
                                            "-q";
   --        /NOQUIET (D)
   --        /QUIET
   --
   --   Work quietly, only output warnings and errors.

   S_Chop_Ref    : aliased constant S := "/REFERENCE "                     &
                                            "-r";
   --        /NOREFERENCE (D)
   --        /REFERENCE
   --
   --   Generate "Source_Reference" pragmas. Use this qualifier if the output
   --   files are regarded as temporary and development is to be done in terms
   --   of the original unchopped file. The /REFERENCE qualifier causes
   --   "Source_Reference" pragmas to be inserted into each of the generated
   --   files to refers back to the original file name and line number.  The
   --   result is that all error messages refer back to the original unchopped
   --   file.
   --
   --   In addition, the debugging information placed into the object file
   --   (when the /DEBUG qualifier of GNAT COMPILE or GNAT MAKE is specified)
   --   also refers back to this original file so that tools like profilers
   --   and debuggers will give information in terms of the original unchopped
   --   file.

   S_Chop_Verb   : aliased constant S := "/VERBOSE "                       &
                                            "-v";
   --        /NOVERBOSE (D)
   --        /VERBOSE
   --
   --   The version number and copyright notice are output, as well as exact
   --   copies of the gnat1 commands spawned to obtain the chop control
   --   information.

   Chop_Switches : aliased constant Switches :=
                     (S_Chop_Comp   'Access,
                      S_Chop_File   'Access,
                      S_Chop_Help   'Access,
                      S_Chop_Over   'Access,
                      S_Chop_Pres   'Access,
                      S_Chop_Quiet  'Access,
                      S_Chop_Ref    'Access,
                      S_Chop_Verb   'Access);

   -----------------------------
   -- Switches for GNAT CLEAN --
   -----------------------------

   S_Clean_Add    : aliased constant S := "/ADD_PROJECT_SEARCH_DIR=*"      &
                                            "-aP*";
   --        /ADD_PROJECT_SEARCH_PATH=(directory[,...])
   --
   --   Add directories to the project search path.

   S_Clean_Compil  : aliased constant S := "/COMPILER_FILES_ONLY "         &
                                             "-c";
   --        /NOCOMPILER_FILES_ONLY (D)
   --        /COMPILER_FILES_ONLY
   --
   --   Only attempt to delete the files produced by the compiler, not those
   --   produced by the binder or the linker. The files that are not to be
   --   deleted are library files, interface copy files, binder generated files
   --   and executable files.

   S_Clean_Current : aliased constant S := "/CURRENT_DIRECTORY "           &
                                            "!-I-";
   --        /CURRENT_DIRECTORY (D)
   --
   --   Look for ALI or object files in the directory where GNAT CLEAN was
   --   invoked.
   --
   --        /NOCURRENT_DIRECTORY
   --
   --   Do not look for ALI or object files in the directory where GNAT CLEAN
   --   was invoked.

   S_Clean_Delete  : aliased constant S := "/DELETE "                      &
                                            "!-n";
   --        /DELETE (D)
   --
   --   Delete the files that are not read-only.
   --
   --        /NODELETE
   --
   --   Informative-only mode. Do not delete any files. Output the list of the
   --   files that would have been deleted if this switch was not specified.

   S_Clean_Dirobj  : aliased constant S := "/DIRECTORY_OBJECTS=@"          &
                                            "-D@";
   --        /DIRECTORY_OBJECTS=<file>
   --
   --   Find the object files and .ALI files in <file>.
   --   This qualifier is not compatible with /PROJECT_FILE.

   S_Clean_Ext     : aliased constant S := "/EXTERNAL_REFERENCE=" & '"'    &
                                            "-X" & '"';
   --        /EXTERNAL_REFERENCE="name=val"
   --
   --   Specifies an external reference to the project manager. Useful only if
   --   /PROJECT_FILE is used.
   --
   --   Example:
   --      /EXTERNAL_REFERENCE="DEBUG=TRUE"

   S_Clean_Follow : aliased constant S := "/FOLLOW_LINKS_FOR_FILES "       &
                                            "-eL";
   --        /NOFOLLOW_LINKS_FOR_FILES (D)
   --        /FOLLOW_LINKS_FOR_FILES
   --
   --    Follow links when parsing project files

   S_Clean_Full    : aliased constant S := "/FULL_PATH_IN_BRIEF_MESSAGES " &
                                            "-F";
   --        /NOFULL_PATH_IN_BRIEF_MESSAGES (D)
   --        /FULL_PATH_IN_BRIEF_MESSAGES
   --
   --   When using project files, if some errors or warnings are detected
   --   during parsing and verbose mode is not in effect (no use of qualifier
   --   /VERBOSE), then error lines start with the full path name of the
   --   project file, rather than its simple file name.

   S_Clean_Help    : aliased constant S := "/HELP "                        &
                                            "-h";
   --        /NOHELP (D)
   --        /HELP
   --
   --   Output a message explaining the usage of gnatclean.

   S_Clean_Index   : aliased constant S := "/SOURCE_INDEX=#"               &
                                             "-i#";
   --        /SOURCE_INDEX=nnn
   --
   --   Specifies the index of the units in the source file
   --   By default, source files are mono-unit and there is no index

   S_Clean_Mess    : aliased constant S := "/MESSAGES_PROJECT_FILE="       &
                                            "DEFAULT "                     &
                                               "-vP0 "                     &
                                            "MEDIUM "                      &
                                               "-vP1 "                     &
                                            "HIGH "                        &
                                               "-vP2";
   --        /MESSAGES_PROJECT_FILE[=messages-option]
   --
   --   Specifies the "verbosity" of the parsing of project files.
   --   messages-option may be one of the following:
   --
   --      DEFAULT (D) No messages are output if there is no error or warning.
   --
   --      MEDIUM      A small number of messages are output.
   --
   --      HIGH        A great number of messages are output, most of them not
   --                  being useful for the user.

   S_Clean_Object  : aliased constant S := "/OBJECT_SEARCH=*"              &
                                            "-aO*";
   --        /OBJECT_SEARCH=(directory,...)
   --
   --   When searching for library and object files, look in the specified
   --   directories. The order in which library files are searched is the same
   --   as for MAKE.

   S_Clean_Project : aliased constant S := "/PROJECT_FILE=<"               &
                                            "-P>";
   --        /PROJECT_FILE=filename
   --
   --   Specifies the main project file to be used. The project files rooted
   --   at the main project file will be parsed before the invocation of the
   --   compiler. The source and object directories to be searched will be
   --   communicated to gnatclean through logical names ADA_PRJ_INCLUDE_FILE
   --   and ADA_PRJ_OBJECTS_FILE.

   S_Clean_Quiet   : aliased constant S := "/QUIET "                       &
                                            "-q";
   --        /NOQUIET (D)
   --        /QUIET
   --
   --   Quiet output. If there are no error, do not output anything, except in
   --   verbose mode (qualifier /VERBOSE) or in informative-only mode
   --  (qualifier /NODELETE).

   S_Clean_Recurs  : aliased constant S := "/RECURSIVE "                   &
                                            "-r";
   --        /NORECURSIVE (D)
   --        /RECURSIVE
   --
   --   When a project file is specified (using switch -P), clean all imported
   --   and extended project files, recursively. If this qualifier is not
   --   specified, only the files related to the main project file are to be
   --   deleted. This qualifier has no effect if no project file is specified.

   S_Clean_Search  : aliased constant S := "/SEARCH=*"                     &
                                            "-I*";
   --        /SEARCH=(directory,...)
   --
   --   Equivalent to /OBJECT_SEARCH=(directory,...).

   S_Clean_Subdirs : aliased constant S := "/SUBDIRS=<"                    &
                                              "--subdirs=>";
   --        /SUBDIRS=dir
   --
   --   The actual directories (object, exec, library, ...) are subdirectories
   --   of the directory specified in the project file. If the subdirectory
   --   does not exist, it is created automatically.

   S_Clean_USL : aliased constant S :=  "/UNCHECKED_SHARED_LIB_IMPORTS " &
                                           "--unchecked-shared-lib-imports";
   --        /NOUNCHECKED_SHARED_LIB_IMPORTS (D)
   --        /UNCHECKED_SHARED_LIB_IMPORTS
   --
   --   Allow shared library projects to import static library projects

   S_Clean_Verbose : aliased constant S := "/VERBOSE "                     &
                                            "-v";
   --        /NOVERBOSE (D)
   --        /VERBOSE
   --
   --   Verbose mode.

   Clean_Switches : aliased constant Switches :=
                      (S_Clean_Add    'Access,
                       S_Clean_Compil 'Access,
                       S_Clean_Current'Access,
                       S_Clean_Delete 'Access,
                       S_Clean_Dirobj 'Access,
                       S_Clean_Ext    'Access,
                       S_Clean_Follow 'Access,
                       S_Clean_Full   'Access,
                       S_Clean_Help   'Access,
                       S_Clean_Index  'Access,
                       S_Clean_Mess   'Access,
                       S_Clean_Object 'Access,
                       S_Clean_Project'Access,
                       S_Clean_Quiet  'Access,
                       S_Clean_Recurs 'Access,
                       S_Clean_Search 'Access,
                       S_Clean_Subdirs'Access,
                       S_Clean_Verbose'Access,
                       S_Clean_USL    'Access);

   -------------------------------
   -- Switches for GNAT COMPILE --
   -------------------------------

   S_GCC_Ada_83  : aliased constant S := "/83 "                            &
                                             "-gnat83";
   --        /NO83 (D)
   --        /83
   --
   --   Although GNAT is primarily an Ada 95 compiler, it accepts this
   --   qualifier to specify that an Ada 83 mode program is being compiled. If
   --   you specify this qualifier, GNAT rejects Ada 95 extensions and applies
   --   Ada 83 semantics. It is not possible to guarantee this qualifier does
   --   a perfect job; for example, some subtle tests of pathological cases,
   --   such as are found in ACVC tests that have been removed from the ACVC
   --   suite for Ada 95, may not compile correctly. However for practical
   --   purposes, using this qualifier should ensure that programs that
   --   compile correctly under the /83 qualifier can be ported reasonably
   --   easily to an Ada 83 compiler. This is the main use of this qualifier.
   --
   --   With few exceptions (most notably the need to use "<>" on
   --   unconstrained generic formal parameters), it is not necessary to use
   --   this qualifier switch when compiling Ada 83 programs, because, with
   --   rare and obscure exceptions, Ada 95 is upwardly compatible with Ada
   --   83. This means that a correct Ada 83 program is usually also a correct
   --   Ada 95 program.

   S_GCC_Ada_95  : aliased constant S := "/95 "                            &
                                             "-gnat95";
   --        /95 (D)
   --
   --   Allows GNAT to recognize the full range of Ada 95 constructs.
   --   This is the normal default for GNAT Pro.

   S_GCC_Ada_05 : aliased constant S := "/05 "                             &
                                             "-gnat05";
   --        /05 (D)
   --
   --   Allows GNAT to recognize the full range of Ada 2005 constructs.

   S_GCC_Ada_2005 : aliased constant S := "/2005 "                         &
                                             "-gnat2005";
   --        /05 (D)
   --
   --   Allows GNAT to recognize the full range of Ada 2005 constructs.
   --   Equivalent to /05 (/2005 is the preferred usage).

   S_GCC_Ada_12 : aliased constant S := "/12 "                             &
                                             "-gnat12";
   --        /05 (D)
   --
   --   Allows GNAT to recognize all implemented proposed Ada 2012
   --   extensions. See features file for list of implemented features.

   S_GCC_Ada_2012 : aliased constant S := "/2012 "                         &
                                             "-gnat2012";
   --        /05 (D)
   --
   --   Allows GNAT to recognize all implemented proposed Ada 2012
   --   extensions. See features file for list of implemented features.
   --   Equivalent to /12 (/2012 is the preferred usage).

   S_GCC_Add     : aliased constant S := "/ADD_PROJECT_SEARCH_DIR=*"       &
                                            "-aP*";
   --        /ADD_PROJECT_SEARCH_PATH=(directory[,...])
   --
   --   Add directories to the project search path.

   S_GCC_Asm     : aliased constant S := "/ASM "                           &
                                             "-S,!-c";
   --        /NOASM (D)
   --        /ASM
   --
   --   Use to cause the assembler source file to be generated, using S as the
   --   filetype, instead of the object file. This may be useful if you need
   --   to examine the generated assembly code.

   S_GCC_AValid  : aliased constant S := "/ASSUME_VALID "                  &
                                             "-gnatB";
   --        /NO_ASSUME_VALID (D)
   --        /ASSUME_VALID
   --
   --   Use to tell the compiler to assume that all objects have valid values
   --   except those occurring as prefixes to 'Valid attributes. In the default
   --   mode, the compiler assumes that values may be invalid unless it can
   --   be sure that they are valid, and code is generated to allow for this
   --   possibility. The use of /ASSUME_VALID will improve the code.

   S_GCC_CategW  : aliased constant S := "/CATEGORIZATION_WARNINGS "  &
                                             "-gnateP";
   --        /NO_CATEGORIZATION_WARNINGS (D)
   --        /CATEGORIZATION_WARNINGS
   --
   --   Use to tell the compiler to disable categorization dependency errors.
   --   Ada requires that units that WITH one another have compatible
   --   categories, for example a Pure unit cannot WITH a Preelaborate unit.
   --   If this switch is used, these errors become warnings (which can be
   --   ignored, or suppressed in the usual manner). This can be useful in
   --   some specialized circumstances such as the temporary use of special
   --   test software.

   S_GCC_Checks  : aliased constant S := "/CHECKS="                        &
                                             "FULL "                       &
                                                "-gnato,!-gnatE,!-gnatp "  &
                                             "OVERFLOW "                   &
                                                "-gnato "                  &
                                             "ELABORATION "                &
                                                "-gnatE "                  &
                                             "ASSERTIONS "                 &
                                                "-gnata "                  &
                                             "DEFAULT "                    &
                                                "!-gnato,!-gnatp "         &
                                             "STACK "                      &
                                                "-fstack-check "           &
                                             "SUPPRESS_ALL "               &
                                                "-gnatp "                  &
                                             "UNSUPPRESS_ALL "             &
                                                "-gnat-p";
   --        /NOCHECKS
   --        /CHECKS[=(keyword[,...])]
   --
   --   If you compile with the default options, GNAT will insert many runtime
   --   checks into the compiled code, including code that performs range
   --   checking against constraints, but not arithmetic overflow checking for
   --   integer operations (including division by zero) or checks for access
   --   before elaboration on subprogram calls.  All other runtime checks, as
   --   required by the Ada 95 Reference Manual, are generated by default.
   --
   --   You may specify one or more of the following keywords to the /CHECKS
   --   qualifier to modify this behavior:
   --
   --     DEFAULT          The behavior described above. This is the default
   --                      if the /CHECKS qualifier is not present on the
   --                      command line. Same as /NOCHECKS.
   --
   --     OVERFLOW        Enables overflow checking for integer operations and
   --                     checks for access before elaboration on subprogram
   --                     calls. This causes GNAT to generate slower and larger
   --                     executable programs by adding code to check for both
   --                     overflow and division by zero (resulting in raising
   --                     "Constraint_Error" as required by Ada semantics).
   --                     Similarly, GNAT does not generate elaboration check
   --                     by default, and you must specify this keyword to
   --                     enable them.
   --
   --                     Note that this keyword does not affect the code
   --                     generated for any floating-point operations; it
   --                     applies only to integer operations. For the case of
   --                     floating-point, GNAT has the "Machine_Overflows"
   --                     attribute set to "False" and the normal mode of
   --                     operation is to generate IEEE NaN and infinite values
   --                     on overflow or invalid operations (such as dividing
   --                     0.0 by 0.0).
   --
   --     ELABORATION     Enables dynamic checks for access-before-elaboration
   --                     on subprogram calls and generic instantiations.
   --
   --     ASSERTIONS      The pragmas "Assert" and "Debug" normally have no
   --                     effect and are ignored. This keyword causes "Assert"
   --                     and "Debug" pragmas to be activated, as well as
   --                     "Check", "Precondition" and "Postcondition" pragmas.
   --
   --     SUPPRESS_ALL    Suppress all runtime checks as though you have
   --                     "pragma Suppress (all_checks)" in your source. Use
   --                     this switch to improve the performance of the code at
   --                     the expense of safety in the presence of invalid data
   --                     or program bugs.
   --
   --     UNSUPPRESS_ALL  Cancels effect of previous SUPPRESS_ALL.
   --
   --     DEFAULT         Suppress the effect of any option OVERFLOW or
   --                     ASSERTIONS.
   --
   --     FULL (D)        Similar to OVERFLOW, but suppress the effect of any
   --                     option ELABORATION or SUPPRESS_ALL.
   --
   --   These keywords only control the default setting of the checks.  You
   --   may modify them using either "Suppress" (to remove checks) or
   --   "Unsuppress" (to add back suppressed checks) pragmas in your program
   --   source.

   S_GCC_ChecksX : aliased constant S := "/NOCHECKS "                      &
                                             "-gnatp,!-gnato,!-gnatE";
   --  NODOC (see /CHECKS)

   S_GCC_Compres : aliased constant S := "/COMPRESS_NAMES "                &
                                             "-gnatC";
   --        /NOCOMPRESS_NAMES (D)
   --        /COMPRESS_NAMES
   --
   --   Compress debug information and external symbol name table entries.
   --   In the generated debugging information, and also in the case of long
   --   external names, the compiler uses a compression mechanism if the name
   --   is very long.  This compression method uses a checksum, and avoids
   --   trouble on some operating systems which have difficulty with very long
   --   names.

   S_GCC_Config  : aliased constant S := "/CONFIGURATION_PRAGMAS_FILE=<"   &
                                             "-gnatec>";
   --        /CONFIGURATION_PRAGMAS_FILE=file
   --
   --   Specify a configuration pragmas file that need to be taken into account

   S_GCC_Current : aliased constant S := "/CURRENT_DIRECTORY "             &
                                             "!-I-";
   --        /CURRENT_DIRECTORY (D)
   --        /NOCURRENT_DIRECTORY
   --
   --   Look for source files in the default directory.

   S_GCC_Data    : aliased constant S := "/DATA_PREPROCESSING=<"           &
                                            "-gnatep>";
   --        /DATA_PREPROCESSING=file_name
   --
   --   This qualifier indicates to the compiler the file name (without
   --   directory information) of the preprocessor data file to use.
   --   The preprocessor data file should be found in the source directories.
   --
   --   A preprocessing data file is a text file with significant lines
   --   indicating how should be preprocessed either a specific source or all
   --   sources not mentioned in other lines. A significant line is a non
   --   empty, non comment line. Comments are similar to Ada comments.
   --
   --  Each significant line starts with either a literal string or the
   --  character '*'. A literal string is the file name (without directory
   --  information) of the source to preprocess. A character '*' indicates the
   --  preprocessing for all the sources that are not specified explicitly on
   --  other lines. It is an error to have two lines with the same file name
   --  or two lines starting with the character '*'.
   --
   --  After the file name or the character '*', another optional literal
   --  string indicating the file name of the definition file to be used for
   --  preprocessing. (see 15.3 Form of Definitions File. The definition files
   --  are found by the compiler in one of the source directories. In some
   --  cases, when compiling a source in a directory other than the current
   --  directory, if the definition file is in the current directory, it may
   --  be necessary to add the current directory as a source directory through
   --  qualifier "/SEARCH=[]", otherwise the compiler would not find the
   --  definition file.
   --
   --  Then, optionally, switches similar to those of gnatprep may be found.
   --  Those switches are:
   --
   --   -b              Causes both preprocessor lines and the lines deleted by
   --                   preprocessing to be replaced by blank lines, preserving
   --                   the line number. This switch is always implied;
   --                   however, if specified after `-c' it cancels the effect
   --                   of `-c'.
   --
   --   -c              Causes both preprocessor lines and the lines deleted by
   --                   preprocessing to be retained as comments marked with
   --                   the special string "--! ".
   --
   --   -Dsymbol=value  Define or redefine a symbol, associated with value.
   --                   A symbol is an Ada identifier, or an Ada reserved word,
   --                   with the exception of "if", "else", "elsif", "end",
   --                   "and", "or" and "then". value is either a literal
   --                   string, an Ada identifier or any Ada reserved word.
   --                   A symbol declared with this switch replaces a symbol
   --                   with the same name defined in a definition file.
   --
   --   -s              Causes a sorted list of symbol names and values to be
   --                   listed on the standard output file.
   --
   --   -u              Causes undefined symbols to be treated as having the
   --                   value FALSE in the context of a preprocessor test.
   --                   In the absence of this option, an undefined symbol
   --                   in a #if or #elsif test will be treated as an error.
   --
   --   Examples of valid lines in a preprocessor data file:
   --
   --     "toto.adb"  "prep.def" -u
   --     --  preprocess "toto.adb", using definition file "prep.def",
   --     --  undefined symbol are False.
   --
   --     * -c -DVERSION=V101
   --     --  preprocess all other sources without a definition file;
   --     --  suppressed lined are commented; symbol VERSION has the value
   --     --  V101.
   --
   --     "titi.adb" "prep2.def" -s
   --     --  preprocess "titi.adb", using definition file "prep2.def";
   --     --  list all symbols with their values.

   S_GCC_Debug   : aliased constant S := "/DEBUG="                         &
                                            "SYMBOLS "                     &
                                               "-g2 "                      &
                                            "NOSYMBOLS "                   &
                                               "!-g2 "                     &
                                            "TRACEBACK "                   &
                                               "-g1 "                      &
                                            "ALL "                         &
                                               "-g3 "                      &
                                            "NONE "                        &
                                               "-g0 "                      &
                                            "NOTRACEBACK "                 &
                                               "-g0";
   --        /DEBUG[=debug-level]
   --        /NODEBUG
   --
   --   Specifies how much debugging information is to be included in
   --   the resulting object fie.
   --
   --   'debug-level' is one of the following:
   --
   --        SYMBOLS (D)    Include both debugger symbol records and traceback
   --                       in the object file.
   --
   --        ALL            Include debugger symbol records, traceback plus
   --                       extra debug information in the object file.
   --
   --        NONE           Excludes both debugger symbol records and traceback
   --                       from the object file.  Same as /NODEBUG.
   --
   --        TRACEBACK      Includes only traceback records in the object
   --                       file. This is the default when /DEBUG is not used.

   S_GCC_DebugX  : aliased constant S := "/NODEBUG "                       &
                                             "!-g";
   --  NODOC (see /Debug)

   S_GCC_Dist    : aliased constant S := "/DISTRIBUTION_STUBS="            &
                                            "RECEIVER "                    &
                                               "-gnatzr "                  &
                                            "CALLER "                      &
                                              "-gnatzc";
   --        /NODISTRIBUTION_STUBS (D)
   --        /DISTRIBUTION_STUBS[=dist-opt]
   --
   --   'dist-opt' is either RECEIVER (the default) or SENDER and indicates
   --   that stubs for use in distributed programs (see the Distributed
   --   Systems Annex of the Ada RM) should be generated.

   S_GCC_DistX   : aliased constant S := "/NODISTRIBUTION_STUBS "          &
                                            "!-gnatzr,!-gnatzc";
   --  NODOC (see /DISTRIBUTION_STUBS)

   S_GCC_Error   : aliased constant S := "/ERROR_LIMIT=#"                  &
                                            "-gnatm#";
   --        /NOERROR_LIMIT (D)
   --        /ERROR_LIMIT=nnn
   --
   --   NNN is a decimal integer in the range of 1 to 999999 and limits the
   --   number of error messages to be generated to that number.  Once that
   --   number has been reached, the compilation is abandoned.
   --   Specifying 999999 is equivalent to /NOERROR_LIMIT.

   S_GCC_ErrorX  : aliased constant S := "/NOERROR_LIMIT "                 &
                                            "-gnatm999999";
   --  NODOC (see /ERROR_LIMIT)

   S_GCC_Except  : aliased constant S := "/EXTRA_EXCEPTION_INFORMATION "   &
                                            "-gnateE";
   --        /EXTRA_EXCEPTION_INFORMATION
   --
   --   Generate extra information in exception messages, in particular
   --   display extra column information and the value and range associated
   --   with index and range check failures, and extra column information for
   --   access checks.

   S_GCC_Expand  : aliased constant S := "/EXPAND_SOURCE "                 &
                                            "-gnatG";
   --        /NOEXPAND_SOURCE (D)
   --        /EXPAND_SOURCE
   --
   --   Produces a listing of the expanded code in Ada source form. For
   --   example, all tasking constructs are reduced to appropriate run-time
   --   library calls. The maximum line length for the listing 72.

   S_GCC_Lexpand : aliased constant S := "/LEXPAND_SOURCE=#"               &
                                            "-gnatG#";
   --        /LEXPAND_SOURCE=nnn
   --
   --   Produces a listing of the expanded code in Ada source form. For
   --   example, all tasking constructs are reduced to appropriate run-time
   --   library calls. The parameter is the maximum line length for the
   --   listing.

   S_GCC_Extend  : aliased constant S := "/EXTENSIONS_ALLOWED "            &
                                            "-gnatX";
   --        /NOEXTENSIONS_ALLOWED (D)
   --        /EXTENSIONS_ALLOWED
   --
   --   GNAT specific language extensions allowed.

   S_GCC_Ext     : aliased constant S := "/EXTERNAL_REFERENCE=" & '"'      &
                                            "-X" & '"';
   --        /EXTERNAL_REFERENCE="name=val"
   --
   --   Specifies an external reference to the project manager. Useful only if
   --   /PROJECT_FILE is used.
   --
   --   Example:
   --      /EXTERNAL_REFERENCE="DEBUG=TRUE"

   S_GCC_File    : aliased constant S := "/FILE_NAME_MAX_LENGTH=#"         &
                                            "-gnatk#";
   --        /FILE_NAME_MAX_LENGTH=nnn
   --
   --   Activates file name "krunching". NNN, a decimal integer in the range
   --   1-999, indicates the maximum allowable length of a file name (not
   --   including the ADS or ADB filetype. The default is not to enable file
   --   name krunching.

   S_GCC_Follow : aliased constant S := "/FOLLOW_LINKS_FOR_FILES "       &
                                            "-eL";
   --        /NOFOLLOW_LINKS_FOR_FILES (D)
   --        /FOLLOW_LINKS_FOR_FILES
   --
   --    Follow links when parsing project files

   S_GCC_Force   : aliased constant S := "/FORCE_ALI "                     &
                                            "-gnatQ";
   --        /NOFORCE_ALI (D)
   --        /FORCE_ALI
   --
   --   In normal operation mode, the .ALI file is not generated if any
   --   illegalities are detected in the program. The use of this qualifier
   --   forces generation of the .ALI file. This file is marked as being
   --   in error, so it cannot be used for binding purposes, but it does
   --   contain reasonably complete cross-reference information, and thus may
   --   be useful for use by tools (e.g. semantic browsing tools or integrated
   --   development environments) that are driven from the .ALI file.

   S_GCC_Full    : aliased constant S := "/FULL_PATH_IN_BRIEF_MESSAGES "   &
                                            "-gnatef";
   --        /NOFULL_PATH_IN_BRIEF_MESSAGES (D)
   --        /FULL_PATH_IN_BRIEF_MESSAGES
   --
   --   When using project files, if some errors or warnings are detected
   --   during parsing and verbose mode is not in effect (no use of qualifier
   --   /VERBOSE), then error lines start with the full path name of the
   --   project file, rather than its simple file name.

   S_GCC_Generate : aliased constant S := "/GENERATE_PROCESSED_SOURCE "    &
                                             "-gnateG";
   --        /NOGENERATE_PROCESSED_SOURCE (D)
   --        /GENERATE_PROCESSED_SOURCE
   --
   --        Generate a file <source>_prep if the integrated preprocessing
   --        is modifying the source text.

   S_GCC_GNAT    : aliased constant S := "/GNAT_INTERNAL "                 &
                                            "-gnatg";
   --        /NOGNAT_INTERNAL (D)
   --        /GNAT_INTERNAL
   --
   --        Internal GNAT implementation mode. This should not be used for
   --        applications programs, it is intended only for use by the compiler
   --        and its run-time library. For documentation, see the GNAT sources.
   --        Note that it implies /WARNINGS=ALL,ERRORS and /STYLE_CHECKS=GNAT
   --        so that all standard warnings and all standard style options are
   --        turned on. All warnings and style error messages are treated as
   --        errors.

   S_GCC_Help    : aliased constant S := "/HELP "                          &
                                            "-gnath";
   --        /NOHELP (D)
   --        /HELP
   --
   --   Output usage information.

   S_GCC_Ident   : aliased constant S := "/IDENTIFIER_CHARACTER_SET="      &
                                             "DEFAULT "                    &
                                                "-gnati1 "                 &
                                             "1 "                          &
                                                "-gnati1 "                 &
                                             "2 "                          &
                                                "-gnati2 "                 &
                                             "3 "                          &
                                                "-gnati3 "                 &
                                             "4 "                          &
                                                "-gnati4 "                 &
                                             "5 "                          &
                                                "-gnati5 "                 &
                                             "PC "                         &
                                                "-gnatip "                 &
                                             "PC850 "                      &
                                                "-gnati8 "                 &
                                             "FULL_UPPER "                 &
                                                "-gnatif "                 &
                                             "NO_UPPER "                   &
                                                "-gnatin "                 &
                                             "WIDE "                       &
                                                "-gnatiw";
   --        /NOIDENTIFIER_CHARACTER_SET (D)
   --        /IDENTIFIER_CHARACTER_SET=char-set
   --
   --   Normally GNAT recognizes the Latin-1 character set in source program
   --   identifiers, as described in the reference manual. This qualifier
   --   causes GNAT to recognize alternate character sets in identifiers.
   --   'char-set' is one of the following strings indicating the character
   --   set:
   --
   --        DEFAULT (D) Equivalent to 1, below. Also equivalent to
   --                    /NOIDENTIFIER_CHARACTER_SET.
   --
   --        1           The basic character set is Latin-1. This character
   --                    set is defined by ISO standard 8859, part 1. The lower
   --                    half (character codes 16#00# ... 16#7F#) is identical
   --                    to standard ASCII coding, but the upper half is used
   --                    to represent additional characters. This includes
   --                    extended letters used by European languages, such as
   --                    the umlaut used in German.
   --
   --                    You may use any of these extended characters freely
   --                    in character or string literals. In addition, the
   --                    extended characters that represent letters can be
   --                    used in identifiers.
   --
   --        2           Latin-2 letters allowed in identifiers, with uppercase
   --                    and lowercase equivalence.
   --
   --        3           Latin-3 letters allowed in identifiers, with uppercase
   --                    and lower case equivalence.
   --
   --        4           Latin-4 letters allowed in identifiers, with uppercase
   --                    and lower case equivalence.
   --
   --        PC          IBM PC code page 437.  This code page is the normal
   --                    default for PCs in the U.S. It corresponds to the
   --                    original IBM PC character set. This set has some, but
   --                    not all, of the extended Latin-1 letters, but these
   --                    letters do not have the same encoding as Latin-1. In
   --                    this mode, these letters are allowed in identifiers
   --                    with uppercase and lowercase equivalence.
   --
   --        PC850       This code page (850) is a modification of 437 extended
   --                    to include all the Latin-1 letters, but still not with
   --                    the usual Latin-1 encoding. In this mode, all these
   --                    letters are allowed in identifiers with uppercase and
   --                    lower case equivalence.
   --
   --        FULL_UPPER  Any character in the range 80-FF allowed in
   --                    identifiers, and all are considered distinct.  In
   --                    other words, there are no uppercase and lower case
   --                    equivalences in this range.
   --
   --        NO_UPPER    No upper-half characters in the range 80-FF are
   --                    allowed in identifiers.  This gives Ada 95
   --                    compatibility for identifier names.
   --
   --        WIDE        GNAT allows wide character codes to appear in
   --                    character and string literals, and also optionally
   --                    in identifiers.  See the /WIDE_CHARACTER_ENCODING
   --                    qualifier for information on encoding formats.

   S_GCC_IdentX  : aliased constant S := "/NOIDENTIFIER_CHARACTER_SET "    &
                                             "-gnati1";
   --  NODOC (see /IDENTIFIER_CHARACTER_SET)

   S_GCC_Ignore  : aliased constant S := "/IGNORE_REP_CLAUSES "            &
                                             "-gnatI";
   --        /IGNORE_REP_CLAUSES
   --
   --   Causes all representation clauses to be ignored and treated as
   --   comments. Useful when compiling foreign code (for example when ASIS
   --   is used to analyze such code).

   S_GCC_Immed   : aliased constant S := "/IMMEDIATE_ERRORS "              &
                                             "-gnatdO";
   --        /NOIMMEDIATE_ERRORS (D)
   --        /IMMEDIATE_ERRORS
   --
   --   Causes errors to be displayed as soon as they are encountered, rather
   --   than after compilation is terminated. If GNAT terminates prematurely
   --   or goes into an infinite loop, the last error message displayed may
   --   help to pinpoint the culprit.

   S_GCC_Inline  : aliased constant S := "/INLINE="                        &
                                            "PRAGMA "                      &
                                              "-gnatn "                    &
                                            "FULL "                        &
                                              "-gnatN "                    &
                                            "SUPPRESS "                    &
                                              "-fno-inline";
   --        /NOINLINE (D)
   --        /INLINE[=keyword]
   --
   --   Selects the level of inlining for your program.  In the absence of this
   --   qualifier, GNAT does not attempt inlining across units and does not
   --   need to access the bodies of subprograms for which "pragma Inline" is
   --   specified if they are not in the current unit.
   --
   --   The supported keywords are as follows:
   --
   --        PRAGMA (D)  Recognize and process "Inline" pragmas.  However,
   --                    for the inlining to actually occur, optimization
   --                    must be enabled.  This enables inlining across unit
   --                    boundaries, that is, inlining a call in one unit of
   --                    a subprogram declared in a with'ed unit. The compiler
   --                    will access these bodies, creating an extra source
   --                    dependency for the resulting object file, and where
   --                    possible, the call will be inlined.
   --
   --                    This qualifier also turns on full optimization and
   --                    requests GNAT to try to attempt automatic inlining
   --                    of small subprograms within a unit.
   --
   --                    Specifying /OPTIMIZE=NONE will disable the main effect
   --                    of this qualifier, but you may specify other
   --                    optimization options, to get either lower
   --                    (/OPTIMIZE=SOME) or higher (/OPTIMIZE=UNROLL_LOOPS)
   --                    levels of optimization.
   --
   --        FULL        Front end inlining. The front end inlining activated
   --                    by this switch is generally more extensive, and quite
   --                    often more effective than the standard PRAGMA inlining
   --                    mode. It will also generate additional dependencies.
   --
   --        SUPPRESS    Suppresses all inlining, even if other optimization
   --                        or inlining switches are set.

   S_GCC_InlineX : aliased constant S := "/NOINLINE "                      &
                                             "!-gnatn,!-gnatN";
   --  NODOC (see /INLINE)

   S_GCC_Intsrc  : aliased constant S := "/INTERSPERSE_SOURCE "            &
                                             "-gnatL";

   --        /NO_INTERSPERSE_SOURCE (D)
   --        /INTERSPERSE_SOURCE
   --
   --   Causes output from /XDEBUG or /EXPAND_SOURCE to be interspersed with
   --   lines from the original source file, output as comment lines with the
   --   associated line number.

   S_GCC_Just   : aliased constant S := "/JUSTIFY_MESSAGES=#"              &
                                             "-gnatj#";

   --        /NO_JUSTIFY_MESSAGES (D)
   --        /JUSTIFY_MESSAGES=nnn
   --
   --   Causes error messages to be reformatted so that a message and all its
   --   continuation lines count as one warning or error in the statistics on
   --   total errors, and the message is broken down into lines (justified) so
   --   that no line is longer than nnn characters. The default message
   --   behavior (each message counted separately and not reformatted to fit
   --   a particular line length) can be obtained using /NO_JUSTIFY_MESSAGES.

   S_GCC_JustX  : aliased constant S := "/NO_JUSTIFY_MESSAGES "            &
                                             "-gnatj0";

   --  NODOC (see /JUSTIFY_MESSAGES)

   S_GCC_Length  : aliased constant S := "/MAX_LINE_LENGTH=#"              &
                                             "-gnatyM#";
   --        /MAX_LINE_LENGTH=nnn
   --
   --   Set maximum line length.
   --   The length of lines must not exceed the given value nnn.

   S_GCC_List    : aliased constant S := "/LIST "                          &
                                             "-gnatl";
   --        /NOLIST (D)
   --        /LIST
   --
   --   Cause a full listing of the file to be generated. In the case where
   --   a body is compiled, the corresponding spec is also listed, along
   --   with any subunits.

   S_GCC_Machine : aliased constant S := "/MACHINE_CODE_LISTING "          &
                                             "-source-listing";
   --        /NOMACHINE_CODE_LISTING (D)
   --        /MACHINE_CODE_LISTING
   --
   --   Cause a full machine code listing of the file to be generated to
   --   <filename>.lis. Interspersed source is included if the /DEBUG
   --   qualifier is also present.

   S_GCC_Mapping : aliased constant S := "/MAPPING_FILE=<"                 &
                                            "-gnatem>";
   --        /MAPPING_FILE=file_name
   --
   --   Use mapping file file_name
   --
   --   A mapping file is a way to communicate to the compiler two mappings:
   --   from unit names to file names (without any directory information) and
   --   from file names to path names (with full directory information).
   --   These mappings are used by the compiler to short-circuit the path
   --   search.
   --
   --   The use of mapping files is not required for correct operation of the
   --   compiler, but mapping files can improve efficiency, particularly when
   --   sources are read over a slow network connection. In normal operation,
   --   you need not be concerned with the format or use of mapping files,
   --   and /MAPPING_FILE is not a qualifier that you would use explicitly.
   --   It is intended only for use by automatic tools such as GNAT MAKE
   --   running under the project file facility. The description here of the
   --   format of mapping files is provided for completeness and for possible
   --   use by other tools.
   --
   --   A mapping file is a sequence of sets of three lines. In each set, the
   --   first line is the unit name, in lower case, with "%s" appended for
   --   specifications and "%b" appended for bodies; the second line is the
   --   file name; and the third line is the path name.
   --
   --   Example:
   --
   --      main%b
   --      main.2_ada
   --      /gnat/project1/sources/main.2_ada
   --
   --   When qualifier ?MAPPING_FILE is specified, the compiler will create in
   --   memory the two mappings from the specified file. If there is any
   --   problem (non existent file, truncated file or duplicate entries),
   --   no mapping will be created.
   --
   --   Several /MAPPING_FILE qualifiers may be specified; however, only the
   --   last one on the command line will be taken into account.
   --
   --   When using a project file, GNAT MAKE creates a temporary mapping file
   --   and communicates it to the compiler using this switch.

   S_GCC_Multi   : aliased constant S := "/MULTI_UNIT_INDEX=#"             &
                                            "-gnateI#";
   --        /MULTI_UNIT_INDEX=nnn
   --
   --   Specify the index of the unit to compile in a multi-unit source file.

   S_GCC_Mess    : aliased constant S := "/MESSAGES_PROJECT_FILE="         &
                                            "DEFAULT "                     &
                                               "-vP0 "                     &
                                            "MEDIUM "                      &
                                               "-vP1 "                     &
                                            "HIGH "                        &
                                               "-vP2";
   --        /MESSAGES_PROJECT_FILE[=messages-option]
   --
   --   Specifies the "verbosity" of the parsing of project files.
   --   messages-option may be one of the following:
   --
   --      DEFAULT (D) No messages are output if there is no error or warning.
   --
   --      MEDIUM      A small number of messages are output.
   --
   --      HIGH        A great number of messages are output, most of them not
   --                  being useful for the user.

   S_GCC_Nesting  : aliased constant S := "/MAX_NESTING=#"                 &
                                             "-gnatyL#";
   --        /MAX_NESTING=nnn
   --
   --   Set maximum level of nesting of constructs (including subprograms,
   --   loops, blocks, packages, and conditionals).
   --   The level of nesting must not exceed the given value nnn.
   --   A value of zero disable this style check (not enabled by default).

   S_GCC_Noadc   : aliased constant S := "/NO_GNAT_ADC "                   &
                                             "-gnatA";
   --        /NO_GNAT_ADC
   --
   --   Cause the compiler to ignore any configuration pragmas file GNAT.ADC
   --   in the default directory. Implied by qualifier /PROJECT_FILE.
   --   Often used in conjunction with qualifier /CONFIGURATION_PRAGMAS_FILE.

   S_GCC_Noload  : aliased constant S := "/NOLOAD "                        &
                                             "-gnatc";
   --        /NOLOAD
   --
   --   Cause the compiler to operate in semantic check mode with full
   --   checking for all illegalities specified in the reference manual, but
   --   without generation of any source code (no object or ALI file
   --   generated).
   --
   --   Since dependent files must be accessed, you must follow the GNAT
   --   semantic restrictions on file structuring to operate in this mode:
   --
   --   o The needed source files must be accessible.
   --   o Each file must contain only one compilation unit.
   --   o The file name and unit name must match.
   --
   --   The output consists of error messages as appropriate. No object file
   --   or ALI file is generated. The checking corresponds exactly to the
   --   notion of legality in the Ada reference manual.
   --
   --   Any unit can be compiled in semantics-checking-only mode, including
   --   units that would not normally be compiled (generic library units,
   --   subunits, and specifications where a separate body is present).

   S_GCC_Nostinc : aliased constant S := "/NOSTD_INCLUDES "                &
                                             "-nostdinc";
   --        /NOSTD_INCLUDES
   --
   --   Do not look in the default directory for source files of the runtime.

   S_GCC_Nostlib : aliased constant S := "/NOSTD_LIBRARIES "               &
                                            "-nostdlib";
   --        /NOSTD_LIBRARIES
   --
   --    Do not look for library files in the system default directory.

   S_GCC_NoWarnP  : aliased constant S := "/NOWARNING_PRAGMAS "            &
                                             "-gnatd.i";
   --        /NOWARNING_PRAGMAS
   --
   --   Causes all Warnings pragmas to be ignored. Useful to check if the
   --   program has obsolete warnings pragmas that are hiding problems.

   S_GCC_Opt     : aliased constant S := "/OPTIMIZE="                      &
                                            "ALL "                         &
                                               "-O2,!-O0,!-O1,!-O3 "       &
                                            "NONE "                        &
                                               "-O0,!-O1,!-O2,!-O3 "       &
                                            "SOME "                        &
                                               "-O1,!-O0,!-O2,!-O3 "       &
                                            "SPACE "                       &
                                               "-Os,!-O0,!-O1,!-O2,!-O3 "  &
                                            "DEVELOPMENT "                 &
                                               "-O1,!-O0,!-O2,!-O3 "       &
                                            "UNROLL_LOOPS "                &
                                               "-funroll-loops "           &
                                            "NO_STRICT_ALIASING "          &
                                               "-fno-strict-aliasing "     &
                                            "INLINING "                    &
                                               "-O3,!-O0,!-O1,!-O2";
   --        /NOOPTIMIZE (D)
   --        /OPTIMIZE[=(keyword[,...])]
   --
   --   Selects the level of optimization for your program.  The supported
   --   keywords are as follows:
   --
   --      ALL (D)       Perform most optimizations, including those that
   --                    may be expensive.
   --
   --      NONE          Do not do any optimizations. Same as /NOOPTIMIZE.
   --
   --      SOME          Perform some optimizations, but omit ones that
   --                    are costly in compilation time.
   --
   --      SPACE         Optimize space usage
   --
   --      DEVELOPMENT   Same as SOME.
   --
   --      INLINING      Full optimization, and also attempt automatic inlining
   --                    of small subprograms within a unit
   --
   --      UNROLL_LOOPS  Try to unroll loops. This keyword may be specified
   --                    with any keyword above other than NONE. Loop
   --                    unrolling usually, but not always, improves the
   --                    performance of programs.
   --
   --      NO_STRICT_ALIASING
   --                    Suppress aliasing analysis. When optimization is
   --                    enabled (ALL or SOME above), the compiler assumes
   --                    that pointers do in fact point to legitimate values
   --                    of the pointer type (allocated from the proper pool).
   --                    If this assumption is violated, e.g. by the use of
   --                    unchecked conversion, then it may be necessary to
   --                    suppress this assumption using this keyword (which
   --                    may be specified only in conjunction with any
   --                    keyword above, other than NONE).

   S_GCC_OptX    : aliased constant S := "/NOOPTIMIZE "                    &
                                            "-O0,!-O1,!-O2,!-O3";
   --  NODOC (see /OPTIMIZE)

   S_GCC_Output  : aliased constant S := "/OUTPUT_FILE=<"                  &
                                            "-gnatl=>";
   --        /OUTPUT_FILE=fname
   --
   --   This has the same effect as /LIST except that the output is written
   --   to a file instead of to standard output. If the given fname
   --   does not start with a period, then it is the full name of the file
   --   to be written. If fname starts with a period, the name of the file
   --   is the concatenation of to the name of the file being compiled with
   --   fname where the period is replace by an underline. For example, if
   --   file xyz.adb is compiled with -gnatl=.lst, then the output is written
   --   to file xyz.adb_lst.

   S_GCC_Pointer : aliased constant S := "/POINTER_SIZE="                  &
                                            "64 "                          &
                                               "-mmalloc64 "               &
                                            "LONG "                        &
                                               "-mmalloc64 "               &
                                            "32 "                          &
                                               "-mno-malloc64 "            &
                                            "SHORT "                       &
                                               "-mno-malloc64";
   --        /POINTER_SIZE=64 (D)
   --        /POINTER_SIZE[=(keyword[,...])]
   --
   --   Change how pointers and descriptors are allocated. The following
   --   keywords are supported:
   --
   --        64 (D)       Allocate heap pointers in 64bit space except as
   --                     constrained by a 32bit size clause or by
   --                     Convention_C and generate 64bit descriptors for
   --                     Descriptor mechanisms for calling imported
   --                     subprograms and accept both 64bit and 32bit
   --                     descriptors for calls to exported subprograms.
   --
   --        LONG         Equivalent to option 64.
   --
   --        32           Allocate all heap pointers in 32bit space and
   --                     generate 32bit descriptors for Descriptor
   --                     mechanisms for calling imported subprograms.
   --
   --        SHORT        Equivalent to option 32.

   S_GCC_Polling : aliased constant S := "/POLLING "                       &
                                            "-gnatP";
   --        /NOPOLLING (D)
   --        /POLLING
   --
   --   Enable polling. See the description of pragma Polling in the GNAT
   --   Reference Manual for full details.

   S_GCC_Project : aliased constant S := "/PROJECT_FILE=<"                 &
                                            "-P>";
   --        /PROJECT_FILE=filename
   --
   --   Specifies the main project file to be used. The project files rooted
   --   at the main project file will be parsed before the invocation of the
   --   compiler. The source and object directories to be searched will be
   --   communicated to the compiler through logical names
   --   ADA_PRJ_INCLUDE_FILE and ADA_PRJ_OBJECTS_FILE.

   S_GCC_Psta    : aliased constant S := "/PRINT_STANDARD "                &
                                            "-gnatS";
   --        /PRINT_STANDARD
   --
   --   cause the compiler to output a representation of package Standard
   --   in a form very close to standard Ada. It is not quite possible to
   --   do this and remain entirely Standard (since new numeric base types
   --   cannot be created in standard Ada), but the output is easily
   --   readable to any Ada programmer, and is useful to determine the
   --   characteristics of target dependent types in package Standard.

   S_GCC_Reswarn : aliased constant S := "/TREAT_RESTRICTIONS_AS_WARNINGS " &
                                             "-gnatr";

   --        /NO_TREAT_RESTRICTIONS_AS_WARNINGS (D)
   --        /TREAT_RESTRICTIONS_AS_WARNINGS
   --
   --   Causes all restrictions to be treated as warnings (pragma Restriction
   --   treated as Restriction_Warnings, pragma Profile as Profile_Warnings,
   --   and pragma Ravenscar sets restriction warnings instead of restrictions)

   S_GCC_Report  : aliased constant S := "/REPORT_ERRORS="                 &
                                            "VERBOSE "                     &
                                               "-gnatv "                   &
                                            "BRIEF "                       &
                                               "-gnatb "                   &
                                            "FULL "                        &
                                               "-gnatf "                   &
                                            "IMMEDIATE "                   &
                                               "-gnatdO "                  &
                                            "DEFAULT "                     &
                                               "!-gnatb,!-gnatv";
   --        /NOREPORT_ERRORS (D)
   --        /REPORT_ERRORS[=(keyword[,...])]
   --
   --   Change the way errors are reported.  The following keywords are
   --   supported:
   --
   --        VERBOSE (D)  Verbose mode. Full error output with source lines
   --                     to SYS$OUTPUT.
   --
   --        BRIEF        Generate the brief format error messages to
   --                     SYS$OUTPUT as well as the verbose format message or
   --                     full listing.
   --
   --        FULL         Normally, the compiler suppresses error messages that
   --                     are likely to be redundant. This keyword causes all
   --                     error messages to be generated. One particular effect
   --                     is for the case of references to undefined variables.
   --                     If a given variable is referenced several times, the
   --                     normal format of messages produces one error.  With
   --                     FULL, each undefined reference produces a separate
   --                     error message.
   --
   --        IMMEDIATE    Normally, the compiler saves up error messages and
   --                     generates them at the end of compilation in proper
   --                     sequence.  This keyword causes error messages to be
   --                     generated as soon as they are detected. The use of
   --                     IMMEDIATE usually causes error messages to be
   --                     generated out of sequence. Use it when the compiler
   --                     blows up due to an internal error.  In this case, the
   --                     error messages may be lost. Sometimes blowups are
   --                     the result of mishandled error messages, so you may
   --                     want to run with this keyword to determine whether
   --                     any error messages were generated.
   --
   --      DEFAULT        Turn off VERBOSE and BRIEF. Same as /NOREPORT_ERRORS.

   S_GCC_ReportX : aliased constant S := "/NOREPORT_ERRORS "               &
                                            "!-gnatb,!-gnatv";
   --  NODOC (see /REPORT_ERRORS)

   S_GCC_Repinfo : aliased constant S := "/REPRESENTATION_INFO="           &
                                            "DEFAULT "                     &
                                               "-gnatR "                   &
                                            "NONE "                        &
                                               "-gnatR0 "                  &
                                            "ARRAYS "                      &
                                               "-gnatR1 "                  &
                                            "ARRAYS_FILE "                 &
                                               "-gnatR1s "                 &
                                            "OBJECTS "                     &
                                               "-gnatR2 "                  &
                                            "OBJECTS_FILE "                &
                                               "-gnatR2s "                 &
                                            "SYMBOLIC "                    &
                                               "-gnatR3 "                  &
                                            "SYMBOLIC_FILE "               &
                                               "-gnatR3s";
   --        /NOREPRESENTATION_INFO (D)
   --        /REPRESENTATION_INFO[=(keyword[,...])]
   --
   --   This qualifier controls output from the compiler of a listing showing
   --   representation information for declared types and objects.
   --
   --        ARRAYS (D)      Size and alignment information is listed for
   --                        declared array and record types.
   --
   --        ARRAYS_FILE     Similar to ARRAYS, but the output is to a file
   --                        with the name 'file_rep' where 'file' is the name
   --                        of the corresponding source file.
   --
   --        NONE            no information is output (equivalent to omitting
   --                        the /REPRESENTATION_INFO qualifiers).
   --
   --        OBJECTS         Size and alignment information is listed for all
   --                        declared types and objects.
   --
   --        OBJECTS_FILE    Similar to OBJECTS, but the output is to a file
   --                        with the name 'file_rep' where 'file' is the name
   --                        of the corresponding source file.
   --
   --        SYMBOLIC        Symbolic expression information for values that
   --                        are computed at run time for variant records.
   --
   --        SYMBOLIC_FILE   Similar to SYMBOLIC, but the output is to a file
   --                        with the name 'file_rep' where 'file' is the name
   --                        of the corresponding source file.
   --
   --        DEFAULT         Equivalent to ARRAYS.

   S_GCC_RepinfX : aliased constant S := "/NOREPRESENTATION_INFO "         &
                                            "!-gnatR";
   --  NODOC (see /REPRESENTATION_INFO)

   S_GCC_RTS     : aliased constant S := "/RUNTIME_SYSTEM=|"               &
                                            "--RTS=|";
   --        /RUNTIME_SYSTEM=xxx
   --
   --    Build against an alternate runtime system named xxx or RTS-xxx.

   S_GCC_SCO     : aliased constant S := "/SCO_OUTPUT "   &
                                            "-gnateS";
   --        /NOSCO_OUTPUT (D)
   --        /SCO_OUTPUT
   --
   --   Controls the output of SCO (Source Coverage Obligation) information
   --   in the generated ALI file. This information is used by advanced source
   --   coverage tools. For a full description of the SCO format, see unit
   --   SCOs in the compiler sources (sco.ads/sco.adb).

   S_GCC_Search  : aliased constant S := "/SEARCH=*"                       &
                                            "-I*";
   --        /SEARCH=(directory[,...])
   --
   --    When looking for source files also look in directories specified.

   S_GCC_Src_Info : aliased constant S := "/SRC_INFO=<"                    &
                                             "--source-info=>";
   --        /SRC_INFO=source-info-file
   --
   --   Specify a source info file to be read or written by the Project
   --   Manager when project files are used.

   S_GCC_Style   : aliased constant S := "/STYLE_CHECKS="                  &
                                            "ALL_BUILTIN "                 &
                                               "-gnatyy "                  &
                                            "0 "                           &
                                               "-gnaty0 "                  &
                                            "1 "                           &
                                               "-gnaty1 "                  &
                                            "2 "                           &
                                               "-gnaty2 "                  &
                                            "3 "                           &
                                               "-gnaty3 "                  &
                                            "4 "                           &
                                               "-gnaty4 "                  &
                                            "5 "                           &
                                               "-gnaty5 "                  &
                                            "6 "                           &
                                               "-gnaty6 "                  &
                                            "7 "                           &
                                               "-gnaty7 "                  &
                                            "8 "                           &
                                               "-gnaty8 "                  &
                                            "9 "                           &
                                               "-gnaty9 "                  &
                                            "ATTRIBUTE "                   &
                                               "-gnatya "                  &
                                            "NOATTRIBUTE "                 &
                                               "-gnaty-a "                 &
                                            "ARRAY_INDEXES "               &
                                               "-gnatyA "                  &
                                            "NOARRAY_INDEXES "             &
                                               "-gnaty-A "                 &
                                            "BLANKS "                      &
                                               "-gnatyb "                  &
                                            "NOBLANKS "                    &
                                               "-gnaty-b "                 &
                                            "BOOLEAN_OPERATORS "           &
                                               "-gnatyB "                  &
                                            "NOBOOLEAN_OPERATORS "         &
                                               "-gnaty-B "                 &
                                            "COMMENTS "                    &
                                               "-gnatyc "                  &
                                            "COMMENTS1 "                   &
                                               "-gnatyC "                  &
                                            "COMMENTS2 "                   &
                                               "-gnatyc "                  &
                                            "NOCOMMENTS "                  &
                                               "-gnaty-c "                 &
                                            "DOS_LINE_ENDINGS "            &
                                               "-gnatyd "                  &
                                            "NODOS_LINE_ENDINGS "          &
                                               "-gnaty-d "                 &
                                            "END "                         &
                                               "-gnatye "                  &
                                            "NOEND "                       &
                                               "-gnaty-e "                 &
                                            "VTABS "                       &
                                               "-gnatyf "                  &
                                            "NOVTABS "                     &
                                               "-gnaty-f "                 &
                                            "GNAT "                        &
                                               "-gnatyg "                  &
                                            "HTABS "                       &
                                               "-gnatyh "                  &
                                            "NOHTABS "                     &
                                               "-gnaty-h "                 &
                                            "IF_THEN "                     &
                                               "-gnatyi "                  &
                                            "NOIF_THEN "                   &
                                               "-gnaty-i "                 &
                                            "KEYWORD "                     &
                                               "-gnatyk "                  &
                                            "NOKEYWORD "                   &
                                               "-gnaty-k "                 &
                                            "LAYOUT "                      &
                                               "-gnatyl "                  &
                                            "NOLAYOUT "                    &
                                               "-gnaty-l "                 &
                                            "LINE_LENGTH "                 &
                                               "-gnatym "                  &
                                            "NOLINE_LENGTH "               &
                                               "-gnaty-m "                 &
                                            "MODE_IN "                     &
                                               "-gnatyI "                  &
                                            "NOMODE_IN "                   &
                                               "-gnaty-I "                 &
                                            "NONE "                        &
                                               "-gnatyN "                  &
                                            "STANDARD_CASING "             &
                                               "-gnatyn "                  &
                                            "NOSTANDARD_CASING "           &
                                               "-gnaty-n "                 &
                                            "ORDERED_SUBPROGRAMS "         &
                                               "-gnatyo "                  &
                                            "NOORDERED_SUBPROGRAMS "       &
                                               "-gnaty-o "                 &
                                            "OVERRIDING_INDICATORS "       &
                                               "-gnatyO "                  &
                                            "NOOVERRIDING_INDICATORS "     &
                                               "-gnaty-O "                 &
                                            "PRAGMA "                      &
                                               "-gnatyp "                  &
                                            "NOPRAGMA "                    &
                                               "-gnaty-p "                 &
                                            "REFERENCES "                  &
                                               "-gnatyr "                  &
                                            "NOREFERENCES "                &
                                               "-gnaty-r "                 &
                                            "SPECS "                       &
                                               "-gnatys "                  &
                                            "NOSPECS "                     &
                                               "-gnaty-s "                 &
                                            "STATEMENTS_AFTER_THEN_ELSE "  &
                                               "-gnatyS "                  &
                                            "NOSTATEMENTS_AFTER_THEN_ELSE " &
                                               "-gnaty-S "                 &
                                            "TOKEN "                       &
                                               "-gnatyt "                  &
                                            "NOTOKEN "                     &
                                               "-gnaty-t "                 &
                                            "UNNECESSARY_BLANK_LINES "     &
                                               "-gnatyu "                  &
                                            "NOUNNECESSARY_BLANK_LINES "   &
                                               "-gnaty-u "                 &
                                            "XTRA_PARENS "                 &
                                               "-gnaty-x "                 &
                                            "NOXTRA_PARENS "               &
                                               "-gnaty-x ";
   --        /NOSTYLE_CHECKS (D)
   --        /STYLE_CHECKS[=(keyword,[...])]
   --
   --   Normally, GNAT permits any code layout consistent with the reference
   --   manual requirements.  This qualifier imposes style checking on the
   --   input source code.  The following keywords are supported:
   --
   --      ALL_BUILTIN (D)      Equivalent to the following list of options:
   --                           3, ATTRIBUTE, BLANKS, COMMENTS2, END, VTABS,
   --                           HTABS, IF_THEN, KEYWORD, LAYOUT, LINE_LENGTH,
   --                           PRAGMA, REFERENCES, SPECS, TOKEN.
   --
   --      1 .. 9               Specify indentation level from 1 to 9.
   --                           The general style of required indentation is as
   --                           specified by the examples in the Ada Reference
   --                           Manual. Full line comments must be aligned with
   --                           the -- starting on a column that is a multiple
   --                           of the alignment level.
   --
   --      ATTRIBUTE            Check attribute casing.
   --                           Attribute names, including the case of keywords
   --                           such as digits used as attributes names,
   --                           must be written in mixed case, that is,
   --                           the initial letter and any letter following an
   --                           underscore must be uppercase.
   --                           All other letters must be lowercase.
   --
   --      ARRAY_INDEXES        Check indexes of array attributes.
   --                           For array attributes First, Last, Range,
   --                           or Length, the index number must be omitted
   --                           for one-dimensional arrays and is required
   --                           for multi-dimensional arrays.
   --
   --      BLANKS               Blanks not allowed at statement end.
   --                           Trailing blanks are not allowed at the end of
   --                           statements. The purpose of this rule, together
   --                           with option HTABS (no horizontal tabs), is to
   --                           enforce a canonical format for the use of
   --                           blanks to separate source tokens.
   --
   --      COMMENTS2            Check comments.
   --      COMMENTS             Comments must meet the following set of rules:
   --
   --                             * The "--" that starts the column must either
   --                               start in column one, or else at least one
   --                               blank must precede this sequence.
   --
   --                             * Comments that follow other tokens on a line
   --                               must have at least one blank following the
   --                               "--" at the start of the comment.
   --
   --                             * Full line comments must have two blanks
   --                               following the "--" that starts the comment,
   --                               with the following exceptions.
   --
   --                             * A line consisting only of the "--"
   --                               characters, possibly preceded by blanks is
   --                               permitted.
   --
   --                             * A comment starting with "--x" where x is
   --                               a special character is permitted. This
   --                               allows proper processing of the output
   --                               generated by specialized tools including
   --                               gnatprep (where --! is used) and the SPARK
   --                               annotation language (where --# is used).
   --                               For the purposes of this rule, a special
   --                               character is defined as being in one of the
   --                               ASCII ranges 16#21#..16#2F# or
   --                               16#3A#..16#3F#.
   --
   --                             * A line consisting entirely of minus signs,
   --                               possibly preceded by blanks, is permitted.
   --                               This allows the construction of box
   --                               comments where lines of minus signs are
   --                               used to form the top and bottom of the box.
   --
   --                             * If a comment starts and ends with "--" is
   --                               permitted as long as at least one blank
   --                               follows the initial "--". Together with
   --                               the preceding rule, this allows the
   --                               construction of box comments, as shown in
   --                               the following example:
   --
   --                               ---------------------------
   --                               -- This is a box comment --
   --                               ---------------------------
   --
   --      COMMENTS1            Check comments (single space).
   --                           Like COMMENTS2, but the -- of a comment only
   --                           requires one or more spaces following, instead
   --                           of two or more spaces.
   --
   --      DOS_LINE_ENDINGS     Check that no DOS line terminators are present
   --                           All lines must be terminated by a single
   --                           ASCII.LF character. In particular the DOS line
   --                           terminator sequence CR / LF is not allowed).
   --
   --      END                  Check end/exit labels.
   --                           Optional labels on end statements ending
   --                           subprograms and on exit statements exiting
   --                           named loops, are required to be present.
   --
   --      GNAT                 Enforces a set of style conventions that
   --                           match the style used in the GNAT source code.
   --                           This maybe useful when developing code that
   --                           is eventually intended to be incorporated into
   --                           GNAT. For further details, see GNAT sources.
   --
   --      HTABS                No horizontal tabs.
   --                           Horizontal tab characters are not permitted in
   --                           the source text. Together with the BLANKS
   --                           (no blanks at end of line) option, this
   --                           enforces a canonical form for the use of blanks
   --                           to separate source tokens.
   --
   --      IF_THEN              Check if-then layout.
   --                           The keyword then must appear either on the
   --                           same line as the corresponding if, or on a line
   --                           on its own, lined up under the if with at least
   --                           one non-blank line in between containing all or
   --                           part of the condition to be tested.
   --
   --      KEYWORD              Check keyword casing.
   --                           All keywords must be in lower case (with the
   --                           exception of keywords such as digits used as
   --                           attribute names to which this check does not
   --                           apply).
   --
   --      LAYOUT               Check layout.
   --                           Layout of statement and declaration constructs
   --                           must follow the recommendations in the Ada
   --                           Reference Manual, as indicated by the form of
   --                           the syntax rules. For example an else keyword
   --                           must be lined up with the corresponding if
   --                           keyword.
   --
   --                           There are two respects in which the style rule
   --                           enforced by this check option are more liberal
   --                           than those in the Ada Reference Manual.
   --                           First in the case of record declarations,
   --                           it is permissible to put the record keyword on
   --                           the same line as the type keyword, and then
   --                           the end in end record must line up under type.
   --                           For example, either of the following two
   --                           layouts is acceptable:
   --
   --                           type q is record
   --                              a : integer;
   --                              b : integer;
   --                           end record;
   --
   --                           type q is
   --                              record
   --                                 a : integer;
   --                                 b : integer;
   --                              end record;
   --
   --                           Second, in the case of a block statement,
   --                           a permitted alternative is to put the block
   --                           label on the same line as the declare or begin
   --                           keyword, and then line the end keyword up under
   --                           the block label. For example both the following
   --                           are permitted:
   --
   --
   --
   --                           Block : declare
   --                              A : Integer := 3;
   --                           begin
   --                              Proc (A, A);
   --                           end Block;
   --
   --                           Block :
   --                              declare
   --                                 A : Integer := 3;
   --                              begin
   --                                 Proc (A, A);
   --                              end Block;
   --
   --                           The same alternative format is allowed for
   --                           loops. For example, both of the following are
   --                           permitted:
   --
   --
   --
   --                           Clear : while J < 10 loop
   --                              A (J) := 0;
   --                           end loop Clear;
   --
   --                           Clear :
   --                              while J < 10 loop
   --                                 A (J) := 0;
   --                              end loop Clear;
   --
   --
   --
   --      LINE_LENGTH          Check maximum line length.
   --                           The length of source lines must not exceed 79
   --                           characters, including any trailing blanks
   --                           The value of 79 allows convenient display on
   --                           an 80 character wide device or window, allowing
   --                           for possible special treatment of 80 character
   --                           lines.
   --
   --      NONE                 Clear any previously set style checks.
   --
   --      ORDERED_SUBPROGRAMS  Check order of subprogram bodies.
   --                           All subprogram bodies in a given scope (e.g.
   --                           a package body) must be in alphabetical order.
   --                           The ordering rule uses normal Ada rules for
   --                           comparing strings, ignoring casing of letters,
   --                           except that if there is a trailing numeric
   --                           suffix, then the value of this suffix is used
   --                           in the ordering (e.g. Junk2 comes before
   --                           Junk10).
   --
   --      OVERRIDING_INDICATORS Check that overriding subprograms are
   --                           explicitly marked as such. The declaration of
   --                           a primitive operation of a type extension that
   --                           overrides an inherited operation must carry
   --                           an overriding indicator.
   --
   --      PRAGMA               Check pragma casing.
   --                           Pragma names must be written in mixed case,
   --                           that is, the initial letter and any letter
   --                           following an underscore must be uppercase.
   --                           All other letters must be lowercase.
   --
   --      REFERENCES           Check references.
   --                           All identifier references must be cased in the
   --                           same way as the corresponding declaration.
   --                           No specific casing style is imposed on
   --                           identifiers. The only requirement is for
   --                           consistency of references with declarations.
   --
   --      SPECS                Check separate specs.
   --                           Separate declarations ("specs") are required
   --                           for subprograms (a body is not allowed to serve
   --                           as its own declaration). The only exception is
   --                           that parameterless library level procedures are
   --                           not required to have a separate declaration.
   --                           This exception covers the most frequent form of
   --                           main program procedures.
   --
   --      STANDARD_CASING      Check casing of entities in Standard.
   --                           Any identifier from Standard must be cased to
   --                           match the presentation in the Ada Reference
   --                           Manual (for example, Integer and ASCII.NUL).
   --
   --      TOKEN                Check token spacing.
   --                           The following token spacing rules are enforced:
   --
   --                             * The keywords abs and not must be followed
   --                               by a space.
   --
   --                             * The token => must be surrounded by spaces.
   --
   --                             * The token <> must be preceded by a space or
   --                               a left parenthesis.
   --
   --                             * Binary operators other than ** must be
   --                               surrounded by spaces. There is no
   --                               restriction on the layout of the ** binary
   --                               operator.
   --
   --                             * Colon must be surrounded by spaces.
   --
   --                             * Colon-equal (assignment) must be surrounded
   --                               by spaces.
   --
   --                             * Comma must be the first non-blank character
   --                               on the line, or be immediately preceded by
   --                               a non-blank character, and must be followed
   --                               by a space.
   --
   --                             * If the token preceding a left paren ends
   --                               with a letter or digit, then a space must
   --                               separate the two tokens.
   --
   --                             * A right parenthesis must either be the
   --                               first non-blank character on a line, or it
   --                               must be preceded by a non-blank character.
   --
   --                             * A semicolon must not be preceded by
   --                               a space, and must not be followed by
   --                               a non-blank character.
   --
   --                             * A unary plus or minus may not be followed
   --                               by a space.
   --
   --                             * A vertical bar must be surrounded by
   --                               spaces.
   --
   --                           In the above rules, appearing in column one is
   --                           always permitted, that is, counts as meeting
   --                           either a requirement for a required preceding
   --                           space, or as meeting a requirement for no
   --                           preceding space.
   --
   --                           Appearing at the end of a line is also always
   --                           permitted, that is, counts as meeting either
   --                           a requirement for a following space,
   --                           or as meeting a requirement for no following
   --                           space.
   --
   --      UNNECESSARY_BLANK_LINES
   --                           Check for unnecessary blank lines.
   --                           A blank line is considered unnecessary if it
   --                           appears at the end of the file, or if more
   --                           than one blank line occurs in sequence.
   --
   --      VTABS                No form feeds or vertical tabs.
   --                           Form feeds or vertical tab characters are not
   --                           permitted in the source text.
   --
   --      XTRA_PARENS          Check for the use of an unnecessary extra
   --                           level of parentheses (C - style) around
   --                           conditions in if statements, while statements
   --                           and exit statements.

   S_GCC_StyleX  : aliased constant S := "/NOSTYLE_CHECKS "                &
                                            "!-gnatg,!-gnaty*";
   --  NODOC (see /STYLE_CHECKS)

   S_GCC_Subdirs : aliased constant S := "/SUBDIRS=<"                      &
                                            "--subdirs=>";
   --        /SUBDIRS=dir
   --
   --   The actual directories (object, exec, library, ...) are subdirectories
   --   of the directory specified in the project file. If the subdirectory
   --   does not exist, it is created automatically.

   S_GCC_Symbol  : aliased constant S := "/SYMBOL_PREPROCESSING=" & '"'    &
                                            "-gnateD" & '"';
   --        /SYMBOL_PREPROCESSING="symbol=value"
   --
   --   Define or redefine a preprocessing symbol, associated with value.
   --   If "=value" is not specified, then the value of the symbol is True.
   --   A symbol is an identifier, following normal Ada (case-insensitive)
   --   rules for its syntax, and value is any sequence (including an empty
   --   sequence) of characters from the set (letters, digits, period,
   --   underline). Ada reserved words may be used as symbols, with the
   --   exceptions of "if", "else", "elsif", "end", "and", "or" and "then".
   --
   --   A symbol declared with this qualifier on the command line replaces
   --   a symbol with the same name either in a definition file or specified
   --   with a switch -D in the preprocessor data file.
   --
   --   This qualifier is similar to qualifier /ASSOCIATE of
   --   GNAT PREPROCESSING.

   S_GCC_Syntax  : aliased constant S := "/SYNTAX_ONLY "                   &
                                            "-gnats";
   --        /NOSYNTAX_ONLY (D)
   --        /SYNTAX_ONLY
   --
   --   Run GNAT in syntax checking only mode.  You can check a series of
   --   files in a single command, and can use wild cards to specify such a
   --   group of files.
   --
   --   You may use other qualifiers in conjunction with this qualifier. In
   --   particular, /LIST and /REPORT_ERRORS=VERBOSE are useful to control the
   --   format of any generated error messages.
   --
   --   The output is simply the error messages, if any. No object file or ALI
   --   file is generated by a syntax-only compilation. Also, no units other
   --   than the one specified are accessed. For example, if a unit "X" with's
   --   a unit "Y", compiling unit "X" in syntax check only mode does not
   --   access the source file containing unit "Y".
   --
   --   Normally, GNAT allows only a single unit in a source file. However,
   --   this restriction does not apply in syntax-check-only mode, and it is
   --   possible to check a file containing multiple compilation units
   --   concatenated together. This is primarily used by the GNAT CHOP
   --   command.

   S_GCC_Table   : aliased constant S := "/TABLE_MULTIPLIER=#"             &
                                            "-gnatT#";
   --        /TABLE_MULTIPLIER=nnn
   --
   --   All compiler tables start at nnn times usual starting size.

   S_GCC_Trace   : aliased constant S := "/TRACE_UNITS "                   &
                                            "-gnatdc";
   --        /TRACE_UNITS
   --        /NOTRACE_UNITS
   --
   --   This switch that does for the frontend what /VERBOSE does for the
   --   backend. The system prints the name of each unit, either a compilation
   --   unit or nested unit, as it is being analyzed.

   S_GCC_Tree    : aliased constant S := "/TREE_OUTPUT "                   &
                                            "-gnatt";
   --        /TREE_OUTPUT
   --        /NOTREE_OUTPUT
   --
   --   Cause GNAT to write the internal tree for a unit to a file (with the
   --   filetype ATB for a body or ATS for a spec).  This is not normally
   --   required, but is used by separate analysis tools. Typically these
   --   tools do the necessary compilations automatically, so you should never
   --   have to specify this switch in normal operation.

   S_GCC_Trys    : aliased constant S := "/TRY_SEMANTICS "                 &
                                            "-gnatq";
   --        /TRY_SEMANTICS
   --        /NOTRY_SEMANTICS
   --
   --   In normal operation mode the compiler first parses the program and
   --   determines if there are any syntax errors. If there are, appropriate
   --   error messages are generated and compilation is immediately
   --   terminated.  This qualifier tells GNAT to continue with semantic
   --   analysis even if syntax errors have been found.  This may enable the
   --   detection of more errors in a single run. On the other hand, the
   --   semantic analyzer is more likely to encounter some internal fatal
   --   error when given a syntactically invalid tree.

   S_GCC_USL : aliased constant S := "/UNCHECKED_SHARED_LIB_IMPORTS "      &
                                         "--unchecked-shared-lib-imports";
   --        /NOUNCHECKED_SHARED_LIB_IMPORTS (D)
   --        /UNCHECKED_SHARED_LIB_IMPORTS
   --
   --   Allow shared library projects to import static library projects

   S_GCC_Units   : aliased constant S := "/UNITS_LIST "                    &
                                            "-gnatu";
   --        /NOUNITS_LIST (D)
   --        /UNITS_LIST
   --
   --   Print a list of units required by this compilation on SYS$OUTPUT.  The
   --   listing includes all units on which the unit being compiled depends
   --   either directly or indirectly.

   S_GCC_Unique  : aliased constant S := "/UNIQUE_ERROR_TAG "              &
                                            "-gnatU";
   --        /NOUNIQUE_ERROR_TAG (D)
   --        /UNIQUE_ERROR_TAG
   --
   --   Tag compiler error messages with the string "error: ".

   S_GCC_Upcase  : aliased constant S := "/UPPERCASE_EXTERNALS "           &
                                            "-gnatF";
   --        /NOUPPERCASE_EXTERNALS (D)
   --        /UPPERCASE_EXTERNALS
   --
   --   Fold default and explicit external names in pragmas Import and Export
   --   to uppercase for compatibility with the default behavior of DEC C.

   S_GCC_Valid   : aliased constant S := "/VALIDITY_CHECKING="             &
                                            "DEFAULT "                     &
                                               "-gnatVd "                  &
                                            "NODEFAULT "                   &
                                               "-gnatVD "                  &
                                            "COPIES "                      &
                                               "-gnatVc "                  &
                                            "NOCOPIES "                    &
                                               "-gnatVC "                  &
                                            "COMPONENTS "                  &
                                               "-gnatVe "                  &
                                            "NOCOMPONENTS "                &
                                               "-gnatVE "                  &
                                            "FLOATS "                      &
                                               "-gnatVf "                  &
                                            "NOFLOATS "                    &
                                               "-gnatVF "                  &
                                            "IN_PARAMS "                   &
                                               "-gnatVi "                  &
                                            "NOIN_PARAMS "                 &
                                               "-gnatVI "                  &
                                            "MOD_PARAMS "                  &
                                               "-gnatVm "                  &
                                            "NOMOD_PARAMS "                &
                                               "-gnatVM "                  &
                                            "OPERANDS "                    &
                                               "-gnatVo "                  &
                                            "NOOPERANDS "                  &
                                               "-gnatVO "                  &
                                            "PARAMETERS "                  &
                                               "-gnatVp "                  &
                                            "NOPARAMETERS "                &
                                               "-gnatVP "                  &
                                            "RETURNS "                     &
                                               "-gnatVr "                  &
                                            "NORETURNS "                   &
                                               "-gnatVR "                  &
                                            "SUBSCRIPTS "                  &
                                               "-gnatVs "                  &
                                            "NOSUBSCRIPTS "                &
                                               "-gnatVS "                  &
                                            "TESTS "                       &
                                               "-gnatVt "                  &
                                            "NOTESTS "                     &
                                               "-gnatVT "                  &
                                            "ALL "                         &
                                               "-gnatVa "                  &
                                            "NONE "                        &
                                               "-gnatVn";
   --        /VALIDITY_CHECKING[=(keyword,[...])]
   --
   --   Control level of validity checking.
   --
   --        DEFAULT (D)     In this mode checks are made to prevent
   --                        erroneous behavior in accordance with the RM.
   --                        Notably extra checks may be needed for case
   --                        statements and subscripted array assignments.
   --
   --        NONE            No special checks for invalid values are
   --                        performed. This means that references to
   --                        uninitialized variables can cause erroneous
   --                        behavior from constructs like case statements
   --                        and subscripted array assignments. In this
   --                        mode, invalid values can lead to erroneous
   --                        behavior.
   --
   --        FULL            Every assignment is checked for validity, so
   --                        that it is impossible to assign invalid values.
   --                        The RM specifically allows such assignments,
   --                        but in this mode, invalid values can never be
   --                        assigned, and an attempt to perform such an
   --                        assignment immediately raises Constraint_Error.
   --                        This behavior is allowed (but not required) by
   --                        the RM. This mode is intended as a debugging aid,
   --                        and may be useful in helping to track down
   --                        uninitialized variables. It may be useful to
   --                        use this in conjunction with the Normalize_Scalars
   --                        pragma which attempts to initialize with invalid
   --                        values where possible.

   S_GCC_Verbose : aliased constant S := "/VERBOSE "                       &
                                            "-v";
   --        /VERBOSE
   --        /NOVERBOSE
   --
   --   Show commands generated by the GCC driver. Normally used only for
   --   debugging purposes or if you need to be sure what version of the
   --   compiler you are executing.

   S_GCC_Verb_Asm : aliased constant S := "/VERBOSE_ASM " &
                                          "-S,-verbose_asm,!-c";
   --        /NOASM (D)
   --        /ASM
   --
   --   Use to cause the assembler source file to be generated, using S as the
   --   filetype, instead of the object file. This may be useful if you need
   --   to examine the generated assembly code.

   S_GCC_Warn    : aliased constant S := "/WARNINGS="                      &
                                            "DEFAULT "                     &
                                               "!-gnatws,!-gnatwe "        &
                                            "ALL "                         &
                                               "-gnatwa "                  &
                                            "EVERY "                       &
                                               "-gnatw.e "                 &
                                            "OPTIONAL "                    &
                                               "-gnatwa "                  &
                                            "NOOPTIONAL "                  &
                                               "-gnatwA "                  &
                                            "NOALL "                       &
                                               "-gnatwA "                  &
                                            "ALL_GCC "                     &
                                               "-Wall "                    &
                                            "FAILING_ASSERTIONS "          &
                                               "-gnatw.a "                 &
                                            "NO_FAILING_ASSERTIONS "       &
                                               "-gnatw.A "                 &
                                            "BAD_FIXED_VALUES "            &
                                               "-gnatwb "                  &
                                            "NO_BAD_FIXED_VALUES "         &
                                               "-gnatwB "                  &
                                            "BIASED_REPRESENTATION "       &
                                               "-gnatw.b "                 &
                                            "NO_BIASED_REPRESENTATION "    &
                                               "-gnatw.B "                 &
                                            "CONDITIONALS "                &
                                               "-gnatwc "                  &
                                            "NOCONDITIONALS "              &
                                               "-gnatwC "                  &
                                            "MISSING_COMPONENT_CLAUSES "   &
                                               "-gnatw.c "                 &
                                            "NOMISSING_COMPONENT_CLAUSES " &
                                               "-gnatw.C "                 &
                                            "IMPLICIT_DEREFERENCE "        &
                                               "-gnatwd "                  &
                                            "NO_IMPLICIT_DEREFERENCE "     &
                                               "-gnatwD "                  &
                                            "ERRORS "                      &
                                               "-gnatwe "                  &
                                            "UNREFERENCED_FORMALS "        &
                                               "-gnatwf "                  &
                                            "NOUNREFERENCED_FORMALS "      &
                                               "-gnatwF "                  &
                                            "UNRECOGNIZED_PRAGMAS "        &
                                               "-gnatwg "                  &
                                            "NOUNRECOGNIZED_PRAGMAS "      &
                                               "-gnatwG "                  &
                                            "HIDING "                      &
                                               "-gnatwh "                  &
                                            "NOHIDING "                    &
                                               "-gnatwH "                  &
                                            "AVOIDGAPS "                   &
                                               "-gnatw.h "                 &
                                            "NOAVOIDGAPS "                 &
                                               "-gnatw.H "                 &
                                            "IMPLEMENTATION "              &
                                               "-gnatwi "                  &
                                            "NOIMPLEMENTATION "            &
                                               "-gnatwI "                  &
                                            "OBSOLESCENT "                 &
                                               "-gnatwj "                  &
                                            "NOOBSOLESCENT "               &
                                               "-gnatwJ "                  &
                                            "CONSTANT_VARIABLES "          &
                                               "-gnatwk "                  &
                                            "NOCONSTANT_VARIABLES "        &
                                               "-gnatwK "                  &
                                            "ELABORATION "                 &
                                               "-gnatwl "                  &
                                            "NOELABORATION "               &
                                               "-gnatwL "                  &
                                            "MODIFIED_UNREF "              &
                                               "-gnatwm "                  &
                                            "NOMODIFIED_UNREF "            &
                                               "-gnatwM "                  &
                                            "SUSPICIOUS_MODULUS "          &
                                               "-gnatw.m "                 &
                                            "NOSUSPICIOUS_MODULUS "        &
                                               "-gnatw.M "                 &
                                            "NORMAL "                      &
                                               "-gnatwn "                  &
                                            "OVERLAYS "                    &
                                               "-gnatwo "                  &
                                            "NOOVERLAYS "                  &
                                               "-gnatwO "                  &
                                            "OUT_PARAM_UNREF "             &
                                               "-gnatw.o "                 &
                                            "NOOUT_PARAM_UNREF "           &
                                               "-gnatw.O "                 &
                                            "INEFFECTIVE_INLINE "          &
                                               "-gnatwp "                  &
                                            "NOINEFFECTIVE_INLINE "        &
                                               "-gnatwP "                  &
                                            "MISSING_PARENS "              &
                                               "-gnatwq "                  &
                                            "PARAMETER_ORDER "             &
                                               "-gnatw.p "                 &
                                            "NOPARAMETER_ORDER "           &
                                               "-gnatw.P "                 &
                                            "NOMISSING_PARENS "            &
                                               "-gnatwQ "                  &
                                            "REDUNDANT "                   &
                                               "-gnatwr "                  &
                                            "NOREDUNDANT "                 &
                                               "-gnatwR "                  &
                                            "OBJECT_RENAMES "              &
                                               "-gnatw.r "                 &
                                            "NOOBJECT_RENAMES "            &
                                               "-gnatw.R "                 &
                                            "SUPPRESS "                    &
                                               "-gnatws "                  &
                                            "OVERRIDING_SIZE "             &
                                               "-gnatw.s "                 &
                                            "NOOVERRIDING_SIZE "           &
                                               "-gnatw.S "                 &
                                            "DELETED_CODE "                &
                                               "-gnatwt "                  &
                                            "NODELETED_CODE "              &
                                               "-gnatwT "                  &
                                            "UNINITIALIZED "               &
                                               "-Wuninitialized "          &
                                            "UNUSED "                      &
                                               "-gnatwu "                  &
                                            "NOUNUSED "                    &
                                               "-gnatwU "                  &
                                            "UNORDERED_ENUMERATIONS "      &
                                               "-gnatw.u "                 &
                                            "NOUNORDERED_ENUMERATIONS "    &
                                               "-gnatw.U "                 &
                                            "VARIABLES_UNINITIALIZED "     &
                                               "-gnatwv "                  &
                                            "NOVARIABLES_UNINITIALIZED "   &
                                               "-gnatwV "                  &
                                            "REVERSE_BIT_ORDER "           &
                                               "-gnatw.v "                 &
                                            "NOREVERSE_BIT_ORDER "         &
                                               "-gnatw.V "                 &
                                            "LOWBOUND_ASSUMED "            &
                                               "-gnatww "                  &
                                            "NOLOWBOUND_ASSUMED "          &
                                               "-gnatwW "                  &
                                            "WARNINGS_OFF_PRAGMAS "        &
                                               "-gnatw.w "                 &
                                            "NO_WARNINGS_OFF_PRAGMAS "     &
                                               "-gnatw.W "                 &
                                            "IMPORT_EXPORT_PRAGMAS "       &
                                               "-gnatwx "                  &
                                            "NOIMPORT_EXPORT_PRAGMAS "     &
                                               "-gnatwX "                  &
                                            "LOCAL_RAISE_HANDLING "        &
                                               "-gnatw.x "                 &
                                            "NOLOCAL_RAISE_HANDLING "      &
                                               "-gnatw.X "                 &
                                            "ADA_2005_COMPATIBILITY "      &
                                               "-gnatwy "                  &
                                            "NOADA_2005_COMPATIBILITY "    &
                                               "-gnatwY "                  &
                                            "UNCHECKED_CONVERSIONS "       &
                                               "-gnatwz "                  &
                                            "NOUNCHECKED_CONVERSIONS "     &
                                               "-gnatwZ";
   --        /NOWARNINGS
   --
   --   Suppress the output of all warning messages from the GNAT front end.
   --   Note that it does not suppress warnings from the gcc back end.
   --
   --        /WARNINGS[=(keyword[,...])]
   --
   --   In addition to error messages, corresponding to illegalities as
   --   defined in the reference manual, the compiler detects two kinds of
   --   warning situations.  First, the compiler considers some constructs
   --   suspicious and generates a warning message to alert you to a possible
   --   error. Second, if the compiler detects a situation that is sure to
   --   raise an exception at runtime, it generates a warning message.
   --
   --   You may specify the following keywords to change this behavior:
   --
   --   DEFAULT (D)             The default behavior above.
   --
   --   ALL                     Activate all optional warnings.
   --                           Activates most optional warning messages,
   --                           see remaining list in this section for
   --                           details on optional warning messages that
   --                           can be individually controlled.
   --                           The warnings that are not turned on by
   --                           this option are BIASED_ROUNDING,
   --                           IMPLICIT_DEREFERENCE, HIDING and
   --                           ELABORATION. All other optional Ada
   --                           warnings are turned on.
   --
   --   EVERY                   Activate every optional warning.
   --                           Activates all optional warnings, including
   --                           those listed above as exceptions for ALL.
   --
   --   NOALL                   Suppress all optional errors.
   --                           Suppresses all optional warning messages
   --                           that can be activated by option ALL.
   --
   --   ALL_GCC                 Request additional messages from the GCC
   --                           backend.  Most of these are not relevant
   --                           to Ada.
   --
   --   CONDITIONALS            Activate warnings for conditional
   --                           Expressions used in tests that are known
   --                           to be True or False at compile time. The
   --                           default is that such warnings are not
   --                           generated.
   --
   --   NOCONDITIONALS          Suppress warnings for conditional
   --                           expressions used in tests that are known
   --                           to be True or False at compile time.
   --
   --   IMPLICIT_DEREFERENCE    Activate warnings on implicit dereferencing.
   --                           The use of a prefix of an access type in an
   --                           indexed component, slice, or selected component
   --                           without an explicit .all will generate
   --                           a warning. With this warning enabled, access
   --                           checks occur only at points where an explicit
   --                           .all appears in the source code (assuming no
   --                           warnings are generated as a result of this
   --                           option). The default is that such warnings are
   --                           not generated. Note that /WARNINGS=ALL does not
   --                           affect the setting of this warning option.
   --
   --   NOIMPLICIT_DEREFERENCE  Suppress warnings on implicit dereferencing.
   --                           in indexed components, slices, and selected
   --                           components.
   --
   --   ELABORATION             Activate warnings on missing pragma
   --                           Elaborate_All statements. The default is
   --                           that such warnings are not generated.
   --
   --   NOELABORATION           Suppress warnings on missing pragma
   --                           Elaborate_All statements.
   --
   --   ERRORS                  Warning messages are to be treated as errors.
   --                           The warning string still appears, but the
   --                           warning messages are counted as errors, and
   --                           prevent the generation of an object file.
   --
   --   HIDING                  Activate warnings on hiding declarations.
   --                           A declaration is considered hiding if it is
   --                           for a non-overloadable entity, and it declares
   --                           an entity with the same name as some other
   --                           entity that is directly or use-visible. The
   --                           default is that such warnings are not
   --                           generated.
   --
   --   NOHIDING                Suppress warnings on hiding declarations.
   --
   --   IMPLEMENTATION          Activate warnings for a with of an internal
   --                           GNAT implementation unit, defined as any unit
   --                           from the Ada, Interfaces, GNAT, DEC or
   --                           System hierarchies that is not documented in
   --                           either the Ada Reference Manual or the GNAT
   --                           Programmer's Reference Manual. Such units are
   --                           intended only for internal implementation
   --                           purposes and should not be with'ed by user
   --                           programs. The default is that such warnings
   --                           are generated.
   --
   --   NOIMPLEMENTATION        Disables warnings for a with of an internal
   --                           GNAT implementation unit.
   --
   --   INEFFECTIVE_INLINE      Activate warnings on ineffective pragma Inlines
   --                           Activates warnings for failure of front end
   --                           inlining (activated by /INLINE=FULL) to inline
   --                           a particular call. There are many reasons for
   --                           not being able to inline a call, including most
   --                           commonly that the call is too complex to
   --                           inline. This warning can also be turned on
   --                           using /INLINE=FULL.
   --
   --   NOINEFFECTIVE_INLINE    Suppress warnings on ineffective pragma Inlines
   --                           Suppresses warnings on ineffective pragma
   --                           Inlines. If the inlining mechanism cannot
   --                           inline a call, it will simply ignore the
   --                           request silently.
   --
   --   MISSING_COMPONENT_CLAUSES
   --                           Activate warnings for cases when there are
   --                           component clauses for a record type, but not
   --                           for every component of the record.
   --
   --   NOMISSING_COMPONENT_CLAUSES
   --                           Suppress warnings for cases when there are
   --                           missing component clauses for a record type.
   --
   --   MISSING_PARENS
   --                           Activate warnings for cases where parentheses
   --                           are not used and the result is potential
   --                           ambiguity from a reader's point of view.
   --                           For example (not a > b) when a and b are
   --                           modular means (not (a) > b) and very likely
   --                           the programmer intended (not (a > b)).
   --
   --   NOMISSING_PARENS
   --                           Suppress warnings for cases where parentheses
   --                           are not used and the result is potential
   --                           ambiguity from a reader's point of view.
   --
   --   MODIFIED_UNREF          Activates warnings for variables that are
   --                           assigned (using an initialization value or with
   --                           one or more assignment statements) but whose
   --                           value is never read. The warning is suppressed
   --                           for volatile variables and also for variables
   --                           that are renamings of other variables or for
   --                           which an address clause is given. This warning
   --                           can also be turned on using /WARNINGS/OPTIONAL.
   --
   --   NOMODIFIED_UNREF        Disables warnings for variables that are
   --                           assigned or initialized, but never read.
   --
   --   NORMAL                  Sets normal warning mode, in which enabled
   --                           warnings are issued and treated as warnings
   --                           rather than errors. This is the default mode.
   --                           It can be used to cancel the effect of an
   --                           explicit /WARNINGS=SUPPRESS or
   --                           /WARNINGS=ERRORS. It also cancels the effect
   --                           of the implicit /WARNINGS=ERRORS that is
   --                           activated by the use of /STYLE=GNAT.
   --
   --   OBSOLESCENT             Activates warnings for calls to subprograms
   --                           marked with pragma Obsolescent and for use of
   --                           features in Annex J of the Ada Reference
   --                           Manual. In the case of Annex J, not all
   --                           features are flagged. In particular use of the
   --                           renamed packages (like Text_IO), use of package
   --                           ASCII and use of the attribute 'Constrained are
   --                           not flagged, since these are very common and
   --                           would generate many annoying positive warnings.
   --                           The default is that such warnings are not
   --                           generated.
   --
   --   NOOBSOLESCENT           Disables warnings on use of obsolescent
   --                           features.
   --
   --   OBJECT_RENAME           Activate warnings for non limited objects
   --                           renaming parameterless functions.
   --
   --   NOOBJECT_RENAME         Suppress warnings for non limited objects
   --                           renaming parameterless functions.
   --
   --   OPTIONAL                Equivalent to ALL.
   --
   --   NOOPTIONAL              Equivalent to NOALL.
   --
   --   OVERLAYS                Activate warnings for possibly unintended
   --                           initialization effects of defining address
   --                           clauses that cause one variable to overlap
   --                           another. The default is that such warnings
   --                           are generated.
   --
   --   NOOVERLAYS              Suppress warnings on possibly unintended
   --                           initialization effects of defining address
   --                           clauses that cause one variable to overlap
   --                           another.
   --
   --   REDUNDANT               Activate warnings for redundant constructs.
   --                           In particular assignments of a variable to
   --                           itself, and a type conversion that converts
   --                           an object to its own type. The default
   --                           is that such warnings are not generated.
   --
   --   NOREDUNDANT             Suppress warnings for redundant constructs.
   --
   --   SUPPRESS                Completely suppress the output of all warning
   --                           messages.  Same as /NOWARNINGS.
   --
   --   UNCHECKED_CONVERSIONS   Activates warnings on unchecked conversions.
   --                           Causes warnings to be generated for
   --                           unchecked conversions when the two types are
   --                           known at compile time to have different sizes.
   --                           The default is that such warnings are
   --                           generated.
   --
   --   NOUNCHECKED_CONVERSIONS Suppress warnings for unchecked conversions.
   --
   --   UNINITIALIZED           Generate warnings for uninitialized variables.
   --                           This is a GCC option, not an Ada option.
   --                           You must also specify the /OPTIMIZE qualifier
   --                           with a value other than NONE (in other words,
   --                           this keyword works only if optimization is
   --                           turned on).
   --
   --   UNREFERENCED_FORMALS    Activate warnings on unreferenced formals.
   --                           Causes a warning to be generated if a formal
   --                           parameter is not referenced in the body of
   --                           the subprogram. This warning can also be turned
   --                           on using option ALL or UNUSED.
   --
   --   NOUNREFERENCED_FORMALS  Suppress warnings on unreferenced formals.
   --                           Suppresses warnings for unreferenced formal
   --                           parameters. Note that the combination UNUSED
   --                           followed by NOUNREFERENCED_FORMALS has the
   --                           effect of warning on unreferenced entities
   --                           other than subprogram formals.
   --
   --   UNUSED                  Activates warnings to be generated for entities
   --                           that are defined but not referenced, and for
   --                           units that are with'ed and not referenced. In
   --                           the case of packages, a warning is also
   --                           generated if no entities in the package are
   --                           referenced. This means that if the package
   --                           is referenced but the only references are in
   --                           use clauses or renames declarations, a warning
   --                           is still generated. A warning is also generated
   --                           for a generic package that is with'ed but never
   --                           instantiated.  In the case where a package or
   --                           subprogram body is compiled, and there is a
   --                           with on the corresponding spec that is only
   --                           referenced in the body, a warning is also
   --                           generated, noting that the with can be moved
   --                           to the body. The default is that such warnings
   --                           are not generated.
   --
   --   NOUNUSED                Suppress warnings for unused entities and
   --                           packages.
   --
   --   VARIABLES_UNINITIALIZED Activates warnings on unassigned variables.
   --                           Causes warnings to be generated when a variable
   --                           is accessed which may not be properly
   --                           uninitialized.
   --                           The default is that such warnings are
   --                           generated.
   --
   --   NOVARIABLES_UNINITIALIZED       Suppress warnings for uninitialized
   --                                   variables.

   S_GCC_WarnX   : aliased constant S := "/NOWARNINGS "                    &
                                            "-gnatws";
   --  NODOC (see /WARNINGS)

   S_GCC_No_Back : aliased constant S := "/NO_BACK_END_WARNINGS "          &
                                            "-w";
   --        /NO_BACK_END_WARNINGS
   --
   --   Inhibit all warning messages of the GCC back-end.

   S_GCC_Wide    : aliased constant S := "/WIDE_CHARACTER_ENCODING="       &
                                             "BRACKETS "                   &
                                                "-gnatWb "                 &
                                             "HEX "                        &
                                                "-gnatWh "                 &
                                             "UPPER "                      &
                                                "-gnatWu "                 &
                                             "SHIFT_JIS "                  &
                                                "-gnatWs "                 &
                                             "UTF8 "                       &
                                                "-gnatW8 "                 &
                                             "EUC "                        &
                                                "-gnatWe";
   --        /NOWIDE_CHARACTER_ENCODING (D)
   --        /WIDE_CHARACTER_ENCODING[=encode-type]
   --
   --   Specifies the mechanism used to encode wide characters.  'encode-type'
   --   is one of the following:
   --
   --   BRACKETS (D)    A wide character is encoded as ["xxxx"] where XXXX
   --                   are four hexadecimal digits representing the coding
   --                   ('Pos value) of the character in type
   --                   Wide_Character. The hexadecimal digits may use upper
   --                   or lower case letters.
   --
   --                   This notation can also be used for upper half
   --                   Character values using the format ["xx"] where XX is
   --                   two hexadecimal digits representing the coding ('Pos
   --                   value) of the character in type Character (or
   --                   Wide_Character). The hexadecimal digits may use upper
   --                   of lower case.
   --
   --   NONE            No wide characters are allowed.  Same
   --                   as /NOWIDE_CHARACTER_ENCODING.
   --
   --   HEX             In this encoding, a wide character is represented by
   --                   the following five character sequence: ESC a b c d
   --                   Where 'a', 'b', 'c', and 'd' are the four hexadecimal
   --                   characters (using uppercase letters) of the wide
   --                   character code. For example, ESC A345 is used to
   --                   represent the wide character with code 16#A345#. This
   --                   scheme is compatible with use of the full
   --                   Wide_Character set.
   --
   --   UPPER           The wide character with encoding 16#abcd# where the
   --                   upper bit is on (in other words, "a" is in the range
   --                   8-F) is represented as two bytes, 16#ab# and 16#cd#.
   --                   The second byte may never be a format control
   --                   character, but is not required to be in the upper
   --                   half. This method can be also used for shift-JIS or
   --                   EUC, where the internal coding matches the external
   --                   coding.
   --
   --   SHIFT_JIS       A wide character is represented by a two-character
   --                   sequence, 16#ab# and 16#cd#, with the restrictions
   --                   described for upper-half encoding as described above.
   --                   The internal character code is the corresponding JIS
   --                   character according to the standard algorithm for
   --                   Shift-JIS conversion. Only characters defined in the
   --                   JIS code set table can be used with this encoding
   --                   method.
   --
   --   UTF8            A wide character is represented using
   --                   UCS Transformation Format 8 (UTF-8) as defined in Annex
   --                   R of ISO 10646-1/Am.2.  Depending on the character
   --                   value, the representation is a one, two, or three byte
   --                   sequence:
   --
   --                   16#0000#-16#007f#: 2#0xxxxxxx#
   --                   16#0080#-16#07ff#: 2#110xxxxx# 2#10xxxxxx#
   --                   16#0800#-16#ffff#: 2#1110xxxx# 2#10xxxxxx# 2#10xxxxxx#
   --
   --                   where the xxx bits correspond to the left-padded bits
   --                   of the 16-bit character value. Note that all lower
   --                   half ASCII characters are represented as ASCII bytes
   --                   and all upper half characters and other wide characters
   --                   are represented as sequences of upper-half (The full
   --                   UTF-8 scheme allows for encoding 31-bit characters as
   --                   6-byte sequences, but in this implementation, all UTF-8
   --                   sequences of four or more bytes length will be treated
   --                   as illegal).
   --
   --   EUC             A wide character is represented by a two-character
   --                   sequence 16#ab# and 16#cd#, with both characters being
   --                   in the upper half. The internal character code is the
   --                   corresponding JIS character according to the EUC
   --                   encoding algorithm. Only characters defined in the JIS
   --                   code set table can be used with this encoding method.

   S_GCC_WideX   : aliased constant S := "/NOWIDE_CHARACTER_ENCODING "     &
                                             "-gnatWn";
   --  NODOC (see /WIDE_CHARACTER_ENCODING)

   S_GCC_Xdebug  : aliased constant S := "/XDEBUG "                        &
                                            "-gnatD";
   --        /NOXDEBUG (D)
   --        /XDEBUG
   --
   --   Output expanded source files for source level debugging.
   --   The expanded source (see /EXPAND_SOURCE) is written to files
   --   with names formed by appending "_DG" to the input file name,
   --   The debugging information generated by the /DEBUG qualifier will then
   --   refer to the generated file. This allows source level debugging using
   --   the generated code which is sometimes useful for complex code, for
   --   example to find out exactly which part of a complex construction
   --   raised an exception. The maximum line length for the output is 72.

   S_GCC_Lxdebug : aliased constant S := "/LXDEBUG=#"                      &
                                            "-gnatD=#";
   --        /LXDEBUG=nnn
   --
   --   Output expanded source files for source level debugging.
   --   The expanded source (see /EXPAND_SOURCE) is written to files
   --   with names formed by appending "_DG" to the input file name,
   --   The debugging information generated by the /DEBUG qualifier will then
   --   refer to the generated file. This allows source level debugging using
   --   the generated code which is sometimes useful for complex code, for
   --   example to find out exactly which part of a complex construction
   --   raised an exception. The parameter is the maximum line length for
   --   the output.

   S_GCC_Xref    : aliased constant S := "/XREF="                          &
                                            "GENERATE "                    &
                                               "!-gnatx "                  &
                                            "SUPPRESS "                    &
                                               "-gnatx";
   --        /XREF[=keyword]
   --
   --   Normally the compiler generates full cross-referencing information in
   --   the .ALI file. This information is used by a number of tools,
   --   including GNAT FIND and GNAT XREF.
   --
   --        GENERATE (D)            Generate cross-referencing information.
   --
   --        SUPPRESS                Suppress cross-referencing information.
   --                                This saves some space and may slightly
   --                                speed up compilation, but means that some
   --                                tools cannot be used.

   GCC_Switches : aliased constant Switches :=
                    (S_GCC_Ada_83  'Access,
                     S_GCC_Ada_95  'Access,
                     S_GCC_Ada_05  'Access,
                     S_GCC_Ada_2005'Access,
                     S_GCC_Ada_12  'Access,
                     S_GCC_Ada_2012'Access,
                     S_GCC_Add     'Access,
                     S_GCC_Asm     'Access,
                     S_GCC_AValid  'Access,
                     S_GCC_CategW  'Access,
                     S_GCC_Checks  'Access,
                     S_GCC_ChecksX 'Access,
                     S_GCC_Compres 'Access,
                     S_GCC_Config  'Access,
                     S_GCC_Current 'Access,
                     S_GCC_Debug   'Access,
                     S_GCC_DebugX  'Access,
                     S_GCC_Data    'Access,
                     S_GCC_Dist    'Access,
                     S_GCC_DistX   'Access,
                     S_GCC_Error   'Access,
                     S_GCC_ErrorX  'Access,
                     S_GCC_Expand  'Access,
                     S_GCC_Lexpand 'Access,
                     S_GCC_Except  'Access,
                     S_GCC_Extend  'Access,
                     S_GCC_Ext     'Access,
                     S_GCC_File    'Access,
                     S_GCC_Follow  'Access,
                     S_GCC_Force   'Access,
                     S_GCC_Full    'Access,
                     S_GCC_Generate'Access,
                     S_GCC_GNAT    'Access,
                     S_GCC_Help    'Access,
                     S_GCC_Ident   'Access,
                     S_GCC_IdentX  'Access,
                     S_GCC_Ignore  'Access,
                     S_GCC_Immed   'Access,
                     S_GCC_Inline  'Access,
                     S_GCC_InlineX 'Access,
                     S_GCC_Intsrc  'Access,
                     S_GCC_Just    'Access,
                     S_GCC_JustX   'Access,
                     S_GCC_Length  'Access,
                     S_GCC_List    'Access,
                     S_GCC_Output  'Access,
                     S_GCC_Machine 'Access,
                     S_GCC_Mapping 'Access,
                     S_GCC_Multi   'Access,
                     S_GCC_Mess    'Access,
                     S_GCC_Nesting 'Access,
                     S_GCC_Noadc   'Access,
                     S_GCC_Noload  'Access,
                     S_GCC_Nostinc 'Access,
                     S_GCC_Nostlib 'Access,
                     S_GCC_NoWarnP 'Access,
                     S_GCC_Opt     'Access,
                     S_GCC_OptX    'Access,
                     S_GCC_Pointer 'Access,
                     S_GCC_Polling 'Access,
                     S_GCC_Project 'Access,
                     S_GCC_Psta    'Access,
                     S_GCC_Report  'Access,
                     S_GCC_ReportX 'Access,
                     S_GCC_Repinfo 'Access,
                     S_GCC_RepinfX 'Access,
                     S_GCC_RTS     'Access,
                     S_GCC_SCO     'Access,
                     S_GCC_Search  'Access,
                     S_GCC_Src_Info'Access,
                     S_GCC_Style   'Access,
                     S_GCC_StyleX  'Access,
                     S_GCC_Subdirs 'Access,
                     S_GCC_Symbol  'Access,
                     S_GCC_Syntax  'Access,
                     S_GCC_Table   'Access,
                     S_GCC_Trace   'Access,
                     S_GCC_Tree    'Access,
                     S_GCC_Trys    'Access,
                     S_GCC_USL     'Access,
                     S_GCC_Units   'Access,
                     S_GCC_Unique  'Access,
                     S_GCC_Upcase  'Access,
                     S_GCC_Valid   'Access,
                     S_GCC_Verbose 'Access,
                     S_GCC_Verb_Asm'Access,
                     S_GCC_Warn    'Access,
                     S_GCC_WarnX   'Access,
                     S_GCC_Wide    'Access,
                     S_GCC_WideX   'Access,
                     S_GCC_No_Back 'Access,
                     S_GCC_Xdebug  'Access,
                     S_GCC_Lxdebug 'Access,
                     S_GCC_Xref    'Access);

   ----------------------------
   -- Switches for GNAT ELIM --
   ----------------------------

   S_Elim_Add    : aliased constant S := "/ADD_PROJECT_SEARCH_DIR=*"       &
                                           "-aP*";
   --        /ADD_PROJECT_SEARCH_PATH=(directory[,...])
   --
   --   Add directories to the project search path.

   S_Elim_All    : aliased constant S := "/ALL "                           &
                                            "-a";
   --        /NOALL (D)
   --        /ALL
   --
   --   Also look for subprograms from the GNAT run time that can be
   --   eliminated. Note that when 'gnat.adc' is produced using this switch,
   --   the entire program must be recompiled with qualifier /ALL_FILES of
   --   GNAT MAKE.

   S_Elim_Bind   : aliased constant S := "/BIND_FILE=<"                    &
                                            "-b>";
   --        /BIND_FILE=file_name
   --
   --   Specifies file_name as the bind file to process. If this qualifier is
   --   not used, the name of the bind file is computed from the full expanded
   --   Ada name of a main subprogram.

   S_Elim_Comp   : aliased constant S := "/COMPILER=@"                     &
                                            "--GCC=@";
   --        /COMPILER=path_name
   --
   --   Instructs GNAT ELIM to use a specific gcc compiler instead of one
   --   available on the path.

   S_Elim_Config : aliased constant S := "/CONFIGURATION_PRAGMAS=<"        &
                                            "-C>";
   --        /CONFIGURATION_PRAGMAS=path_name
   --
   --   Specifies a file that contains configuration pragmas.
   --   The file must be specified with absolute path.

   S_Elim_Current : aliased constant S := "/CURRENT_DIRECTORY "            &
                                           "!-I-";
   --        /CURRENT_DIRECTORY (D)
   --        /NOCURRENT_DIRECTORY
   --
   --        Look for source files in the default directory.

   S_Elim_Ext     : aliased constant S := "/EXTERNAL_REFERENCE=" & '"'     &
                                             "-X" & '"';
   --       /EXTERNAL_REFERENCE="name=val"
   --
   --   Specifies an external reference to the project manager. Useful only if
   --   /PROJECT_FILE is used.
   --
   --   Example:
   --      /EXTERNAL_REFERENCE="DEBUG=TRUE"

   S_Elim_Follow  : aliased constant S := "/FOLLOW_LINKS_FOR_FILES "       &
                                            "-eL";
   --        /NOFOLLOW_LINKS_FOR_FILES (D)
   --        /FOLLOW_LINKS_FOR_FILES
   --
   --    Follow links when parsing project files

   S_Elim_GNATMAKE : aliased constant S := "/GNATMAKE=@"                   &
                                            "--GNATMAKE=@";
   --        /GNATMAKE=path_name
   --
   --   Instructs GNAT MAKE to use a specific gnatmake instead of one available
   --   on the path.

   S_Elim_Mess    : aliased constant S := "/MESSAGES_PROJECT_FILE="        &
                                             "DEFAULT "                    &
                                                "-vP0 "                    &
                                             "MEDIUM "                     &
                                                "-vP1 "                    &
                                             "HIGH "                       &
                                                "-vP2";
   --        /MESSAGES_PROJECT_FILE[=messages-option]
   --
   --   Specifies the "verbosity" of the parsing of project files.
   --   messages-option may be one of the following:
   --
   --      DEFAULT (D)  No messages are output if there is no error or warning.
   --
   --      MEDIUM       A small number of messages are output.
   --
   --      HIGH         A great number of messages are output, most of them not
   --                   being useful for the user.

   S_Elim_Nodisp : aliased constant S := "/NO_DISPATCH "                   &
                                          "--no-elim-dispatch";
   --        /NONO_DISPATCH (D)
   --        /NO_DISPATCH
   --
   --   Do not generate pragmas for dispatching operations.

   S_Elim_Ignore : aliased constant S := "/IGNORE=@"                       &
                                         "--ignore=@";
   --      /IGNORE=filename
   --
   --   Do not generate pragmas for subprograms declared in the sources
   --  listed in a specified file

   S_Elim_Project : aliased constant S := "/PROJECT_FILE=<"                &
                                             "-P>";
   --        /PROJECT_FILE=filename
   --
   --   Specifies the main project file to be used. The project files rooted
   --   at the main project file will be parsed before the invocation of the
   --   gnatelim. The source directories to be searched will be communicated
   --   to gnatelim through logical name ADA_PRJ_INCLUDE_FILE.

   S_Elim_Quiet   : aliased constant S := "/QUIET "                        &
                                            "-q";
   --        /NOQUIET (D)
   --        /QUIET
   --
   --   Quiet mode: by default GNAT ELIM outputs to the standard error stream
   --   the number of program units left to be processed. This option turns
   --   this trace off.

   S_Elim_Files   : aliased constant S := "/FILES=@"                       &
                                         "-files=@";

   --      /FILES=filename
   --
   --   Take as arguments the files that are listed in the specified
   --   text file.

   S_Elim_Log     : aliased constant S := "/LOG "                          &
                                          "-l";
   --        /NOLOG (D)
   --        /LOG
   --
   --   Duplicate all the output sent to Stderr into a default log file.

   S_Elim_Logfile : aliased constant S := "/LOGFILE=@"                     &
                                          "-l@";

   --      /LOGFILE=logfilename
   --
   --   Duplicate all the output sent to Stderr into a specified log file.

   S_Elim_Main    : aliased constant S := "/MAIN=@"                        &
                                          "-main=@";

   --      /MAIN=filename
   --
   --   Specify the main subprogram of the partition to analyse.

   S_Elim_Out     : aliased constant S := "/OUTPUT=@"                     &
                                             "-o@";
   --        /OUTPUT=filename
   --
   --   Specify the name of the output file.

   S_Elim_Time    : aliased constant S := "/TIME "                        &
                                            "-t";
   --        /NOTIME (D)
   --        /TIME
   --
   --   Print out execution time

   S_Elim_Search : aliased constant S := "/SEARCH=*"                       &
                                            "-I*";
   --        /SEARCH=(directory, ...)
   --
   --   When looking for source files also look in the specified directories.

   S_Elim_Subdirs : aliased constant S := "/SUBDIRS=<"                     &
                                             "--subdirs=>";
   --        /SUBDIRS=dir
   --
   --   The actual directories (object, exec, library, ...) are subdirectories
   --   of the directory specified in the project file. If the subdirectory
   --   does not exist, it is created automatically.

   S_Elim_Verb   : aliased constant S := "/VERBOSE "                       &
                                            "-v";
   --        /NOVERBOSE (D)
   --        /VERBOSE
   --
   --   Verbose mode: GNAT ELIM version information is output as Ada comments
   --   to the standard output stream. Also, in addition to the number of
   --   program units left, GNAT ELIM will output the name of the current unit
   --   being processed.

   S_Elim_Warn   : aliased constant S := "/WARNINGS="                      &
                                           "NORMAL "                       &
                                               "-wn "                      &
                                           "QUIET "                        &
                                                "-ws";

   --      /WARNINGS[=(keyword[,...])]
   --
   --   The following keywords are supported:
   --
   --        NORMAL (D)    Print warning all the messages.
   --        QUIET         Some warning messages are suppressed

   Elim_Switches : aliased constant Switches :=
                     (S_Elim_Add     'Access,
                      S_Elim_All     'Access,
                      S_Elim_Bind    'Access,
                      S_Elim_Comp    'Access,
                      S_Elim_Config  'Access,
                      S_Elim_Current 'Access,
                      S_Elim_Ext     'Access,
                      S_Elim_Files   'Access,
                      S_Elim_Follow  'Access,
                      S_Elim_GNATMAKE'Access,
                      S_Elim_Log     'Access,
                      S_Elim_Logfile 'Access,
                      S_Elim_Main    'Access,
                      S_Elim_Mess    'Access,
                      S_Elim_Nodisp  'Access,
                      S_Elim_Out     'Access,
                      S_Elim_Project 'Access,
                      S_Elim_Quiet   'Access,
                      S_Elim_Search  'Access,
                      S_Elim_Subdirs 'Access,
                      S_Elim_Time    'Access,
                      S_Elim_Verb    'Access,
                      S_Elim_Warn    'Access);

   ----------------------------
   -- Switches for GNAT FIND --
   ----------------------------

   S_Find_Add     : aliased constant S := "/ADD_PROJECT_SEARCH_DIR=*"      &
                                            "-aP*";
   --        /ADD_PROJECT_SEARCH_PATH=(directory[,...])
   --
   --   Add directories to the project search path.

   S_Find_All     : aliased constant S := "/ALL_FILES "                    &
                                            "-a";
   --        /NOALL_FILES (D)
   --        /ALL_FILES
   --
   --   If this switch is present, FIND and XREF will parse the read-only
   --   files found in the library search path. Otherwise, these files will
   --   be ignored. This option can be used to protect Gnat sources or your
   --   own libraries from being parsed, thus making FIND and XREF much
   --   faster, and their output much smaller.

   S_Find_Deriv   : aliased constant S := "/DERIVED_TYPE_INFORMATION "     &
                                            "-d";
   --        /NODERIVED_TYPE_INFORMATION (D)
   --        /DERIVED_TYPE_INFORMATION
   --
   --   Output the parent type reference for each matching derived types.

   S_Find_Expr    : aliased constant S := "/EXPRESSIONS "                  &
                                            "-e";
   --        /NOEXPRESSIONS (D)
   --        /EXPRESSIONS
   --
   --   By default, FIND accepts the simple regular expression set for pattern.
   --   If this switch is set, then the pattern will be considered as a full
   --   Unix-style regular expression.

   S_Find_Ext     : aliased constant S := "/EXTERNAL_REFERENCE=" & '"'     &
                                            "-X" & '"';
   --        /EXTERNAL_REFERENCE="name=val"
   --
   --   Specifies an external reference to the project manager. Useful only if
   --   /PROJECT_FILE is used.
   --
   --   Example:
   --      /EXTERNAL_REFERENCE="DEBUG=TRUE"

   S_Find_Follow  : aliased constant S := "/FOLLOW_LINKS_FOR_FILES "       &
                                            "-eL";
   --        /NOFOLLOW_LINKS_FOR_FILES (D)
   --        /FOLLOW_LINKS_FOR_FILES
   --
   --    Follow links when parsing project files

   S_Find_Full    : aliased constant S := "/FULL_PATHNAME "                &
                                            "-f";
   --        /NOFULL_PATHNAME (D)
   --        /FULL_PATHNAME
   --
   --   If this switch is set, the output file names will be preceded by their
   --   directory (if the file was found in the search path). If this switch
   --   is not set, the directory will not be printed.

   S_Find_Ignore  : aliased constant S := "/IGNORE_LOCALS "                &
                                            "-g";
   --        /NOIGNORE_LOCALS (D)
   --        /IGNORE_LOCALS
   --
   --   If this switch is set, information is output only for library-level
   --   entities, ignoring local entities. The use of this switch may
   --   accelerate FIND and XREF.

   S_Find_Mess    : aliased constant S := "/MESSAGES_PROJECT_FILE="        &
                                            "DEFAULT "                     &
                                               "-vP0 "                     &
                                            "MEDIUM "                      &
                                               "-vP1 "                     &
                                            "HIGH "                        &
                                               "-vP2";
   --        /MESSAGES_PROJECT_FILE[=messages-option]
   --
   --   Specifies the "verbosity" of the parsing of project files.
   --   messages-option may be one of the following:
   --
   --      DEFAULT (D)  No messages are output if there is no error or warning.
   --
   --      MEDIUM       A small number of messages are output.
   --
   --      HIGH         A great number of messages are output, most of them not
   --                   being useful for the user.

   S_Find_Nostinc : aliased constant S := "/NOSTD_INCLUDES "               &
                                            "-nostdinc";
   --        /NOSTD_INCLUDES
   --
   --   Do not look for sources in the system default directory.

   S_Find_Nostlib : aliased constant S := "/NOSTD_LIBRARIES "              &
                                            "-nostdlib";
   --        /NOSTD_LIBRARIES
   --
   --   Do not look for library files in the system default directory.

   S_Find_Object  : aliased constant S := "/OBJECT_SEARCH=*"               &
                                            "-aO*";
   --        /OBJECT_SEARCH=(directory,...)
   --
   --   When searching for library and object files, look in the specified
   --   directories. The order in which library files are searched is the same
   --   as for MAKE.

   S_Find_Print   : aliased constant S := "/PRINT_LINES "                  &
                                            "-s";
   --        /NOPRINT_LINES (D)
   --        /PRINT_LINES
   --
   --   Output the content of the Ada source file lines were the entity was
   --   found.

   S_Find_Project : aliased constant S := "/PROJECT=@"                     &
                                            "-p@";
   --        /PROJECT=file
   --
   --   Specify a project file to use. By default, FIND and XREF will try to
   --   locate a project file in the current directory.
   --
   --   If a project file is either specified or found by the tools, then the
   --   content of the source directory and object directory lines are added
   --   as if they had been specified respectively by /SOURCE_SEARCH and
   --   /OBJECT_SEARCH.
   --
   --   This qualifier is not compatible with /PROJECT_FILE

   S_Find_Prj     : aliased constant S := "/PROJECT_FILE=<"                &
                                            "-P>";
   --        /PROJECT_FILE=filename
   --
   --   Specifies the main project file to be used. The project files rooted
   --   at the main project file will be parsed before looking for sources.
   --   The source and object directories to be searched will be communicated
   --   to gnatfind through logical names ADA_PRJ_INCLUDE_FILE and
   --   ADA_PRJ_OBJECTS_FILE.

   S_Find_Ref     : aliased constant S := "/REFERENCES "                   &
                                            "-r";
   --        /NOREFERENCES (D)
   --        /REFERENCES
   --
   --   By default, FIND will output only the information about the
   --   declaration, body or type completion of the entities. If this switch
   --   is set, the FIND will locate every reference to the entities in the
   --   files specified on the command line (or in every file in the search
   --   path if no file is given on the command line).

   S_Find_Search  : aliased constant S := "/SEARCH=*"                      &
                                            "-I*";
   --        /SEARCH=(directory,...)
   --
   --   Equivalent to:
   --   /OBJECT_SEARCH=(directory,...) /SOURCE_SEARCH=(directory,...)

   S_Find_Source  : aliased constant S := "/SOURCE_SEARCH=*"               &
                                            "-aI*";
   --        /SOURCE_SEARCH=(directory,...)
   --
   --   When looking for source files also look in the specified directories.
   --   The order in which source file search is undertaken is the same as for
   --   MAKE.

   S_Find_Subdirs : aliased constant S := "/SUBDIRS=<"                     &
                                             "--subdirs=>";
   --        /SUBDIRS=dir
   --
   --   The actual directories (object, exec, library, ...) are subdirectories
   --   of the directory specified in the project file. If the subdirectory
   --   does not exist, it is created automatically.

   S_Find_Types   : aliased constant S := "/TYPE_HIERARCHY "               &
                                            "-t";
   --        /NOTYPE_HIERARCHY (D)
   --        /TYPE_HIERARCHY
   --
   --   Output the type hierarchy for the specified type. It acts like the
   --   /DERIVED_TYPE_INFORMATION qualifier, but recursively from parent type
   --   to parent type. When this qualifier is specified it is not possible to
   --   specify more than one file.

   Find_Switches : aliased constant Switches :=
                     (S_Find_Add     'Access,
                      S_Find_All     'Access,
                      S_Find_Deriv   'Access,
                      S_Find_Expr    'Access,
                      S_Find_Ext     'Access,
                      S_Find_Follow  'Access,
                      S_Find_Full    'Access,
                      S_Find_Ignore  'Access,
                      S_Find_Mess    'Access,
                      S_Find_Nostinc 'Access,
                      S_Find_Nostlib 'Access,
                      S_Find_Object  'Access,
                      S_Find_Print   'Access,
                      S_Find_Project 'Access,
                      S_Find_Prj     'Access,
                      S_Find_Ref     'Access,
                      S_Find_Search  'Access,
                      S_Find_Source  'Access,
                      S_Find_Subdirs 'Access,
                      S_Find_Types   'Access);

   ------------------------------
   -- Switches for GNAT KRUNCH --
   ------------------------------

   S_Krunch_Count  : aliased constant S := "/COUNT=#"                      &
                                            "`#";
   --        /COUNT=39 (D)
   --        /COUNT=nnn
   --
   --   Limit file names to nnn characters (where nnn is a decimal
   --   integer). The maximum file name length is 39, but if you want to
   --   generate a set of files that would be usable if ported to a system
   --   with some different maximum file length, then a different value can
   --   be specified.

   Krunch_Switches : aliased constant Switches  :=
                       (1 .. 1 => S_Krunch_Count  'Access);

   ----------------------------
   -- Switches for GNAT LINK --
   ----------------------------

   S_Link_Add     : aliased constant S := "/ADD_PROJECT_SEARCH_DIR=*"      &
                                            "-aP*";
   --        /ADD_PROJECT_SEARCH_PATH=(directory[,...])
   --
   --   Add directories to the project search path.

   S_Link_Bind    : aliased constant S := "/BIND_FILE="                    &
                                            "ADA "                         &
                                               "-A "                       &
                                            "C "                           &
                                               "-C";
   --        /BIND_FILE=[bind-file-option]
   --
   --   Specifies the language of the binder generated file.
   --
   --        ADA (D)         Binder file is Ada.
   --
   --        C               Binder file is 'C'.

   S_Link_Debug   : aliased constant S := "/DEBUG="                        &
                                            "ALL "                         &
                                               "-g3 "                      &
                                            "NONE "                        &
                                               "-g0 "                      &
                                            "TRACEBACK "                   &
                                               "-g1 "                      &
                                            "NOTRACEBACK "                 &
                                               "-g0";
   --        /NODEBUG (D)
   --        /DEBUG[=debug-option]
   --
   --   Specifies the amount of debugging information included. 'debug-option'
   --   is one of the following:
   --
   --        ALL (D)      Include full debugging information.
   --
   --        NONE         Provide no debugging information.  Same as /NODEBUG.
   --
   --        TRACEBACK    Provide sufficient debug information for a traceback.
   --
   --        NOTRACEBACK  Same as NONE.

   S_Link_Nodebug : aliased constant S := "/NODEBUG "                      &
                                            "-g0";
   --  NODOC (see /DEBUG)

   S_Link_Execut  : aliased constant S := "/EXECUTABLE=@"                  &
                                            "-o@";
   --        /EXECUTABLE=exec-name
   --
   --   'exec-name' specifies an alternative name for the generated executable
   --   program. If this qualifier switch is omitted, the executable is called
   --   the name of the main unit. So "$ GNAT LINK TRY.ALI" creates an
   --   executable called TRY.EXE.

   S_Link_Ext     : aliased constant S := "/EXTERNAL_REFERENCE=" & '"'     &
                                            "-X" & '"';
   --        /EXTERNAL_REFERENCE="name=val"
   --
   --   Specifies an external reference to the project manager. Useful only if
   --   /PROJECT_FILE is used.
   --
   --   Example:
   --      /EXTERNAL_REFERENCE="DEBUG=TRUE"

   S_Link_Follow  : aliased constant S := "/FOLLOW_LINKS_FOR_FILES "       &
                                            "-eL";
   --        /NOFOLLOW_LINKS_FOR_FILES (D)
   --        /FOLLOW_LINKS_FOR_FILES
   --
   --    Follow links when parsing project files

   S_Link_Forlink : aliased constant S := "/FOR_LINKER=" & '"'             &
                                            "--for-linker=" & '"';
   --        /FOR_LINKER=<string>
   --
   --   Transmit the option <string> to the underlying linker.

   S_Link_Force   : aliased constant S := "/FORCE_OBJECT_FILE_LIST "       &
                                            "-f";
   --        /NOFORCE_OBJECT_FILE_LIST (D)
   --        /FORCE_OBJECT_FILE_LIST
   --
   --   Forces the generation of a file that contains commands for the linker.
   --   This is useful in some cases to deal with special situations where the
   --   command line length is exceeded.

   S_Link_Ident   : aliased constant S := "/IDENTIFICATION=" & '"'         &
                                            "--for-linker=IDENT="          &
                                            '"';
   --        /IDENTIFICATION="<string>"
   --
   --   "<string>" specifies the string to be stored in the image file ident-
   --   ification field in the image header. It overrides any pragma Ident
   --   specified string.

   S_Link_Libdir  : aliased constant S := "/LIBDIR=*"                      &
                                            "-L*";
   --        /LIBDIR=(directory, ...)
   --
   --   Look for libraries in the specified directories.

   S_Link_Library : aliased constant S := "/LIBRARY=|"                     &
                                            "-l|";
   --        /LIBRARY=xyz
   --
   --   Link with library named "xyz".

   S_Link_Mess    : aliased constant S := "/MESSAGES_PROJECT_FILE="        &
                                            "DEFAULT "                     &
                                               "-vP0 "                     &
                                            "MEDIUM "                      &
                                               "-vP1 "                     &
                                            "HIGH "                        &
                                               "-vP2";
   --        /MESSAGES_PROJECT_FILE[=messages-option]
   --
   --   Specifies the "verbosity" of the parsing of project files.
   --   messages-option may be one of the following:
   --
   --      DEFAULT (D)  No messages are output if there is no error or warning.
   --
   --      MEDIUM       A small number of messages are output.
   --
   --      HIGH         A great number of messages are output, most of them not
   --                   being useful for the user.

   S_Link_Nocomp  : aliased constant S := "/NOCOMPILE "                    &
                                            "-n";
   --        /NOCOMPILE
   --
   --   Do not compile the file generated by the binder.
   --   This may be used when a link is rerun with different options,
   --   but there is no need to recompile the binder generated file.

   S_Link_Noinhib : aliased constant S := "/NOINHIBIT-EXEC "               &
                                            "--for-linker=--noinhibit-exec";
   --        /NOINHIBIT-EXEC
   --
   --   Delete executable if there are errors or warnings.

   S_Link_Nofiles : aliased constant S := "/NOSTART_FILES "                &
                                            "-nostartfiles";
   --        /NOSTART_FILES
   --
   --   Link in default image initialization and startup functions.

   S_Link_Project : aliased constant S := "/PROJECT_FILE=<"                &
                                            "-P>";
   --        /PROJECT_FILE=filename
   --
   --   Specifies the main project file to be used. The project files rooted
   --   at the main project file will be parsed before the invocation of the
   --   linker.
   --   The source and object directories to be searched will be communicated
   --   to the linker through logical names ADA_PRJ_INCLUDE_FILE and
   --   ADA_PRJ_OBJECTS_FILE.

   S_Link_Return  : aliased constant S := "/RETURN_CODES="                 &
                                            "POSIX "                       &
                                               "!-mvms-return-codes "      &
                                            "VMS "                         &
                                               "-mvms-return-codes";
   --        /RETURN_CODES=POSIX (D)
   --        /RETURN_CODES=VMS
   --
   --   Specifies the style of codes returned by
   --   Ada.Command_Line.Set_Exit_Status. Must be used in conjunction with
   --   and match the Bind qualifier with the same name.
   --
   --        POSIX (D)   Return Posix compatible exit codes.
   --
   --        VMS         Return VMS compatible exit codes. The value returned
   --                    is identically equal to the Set_Exit_Status parameter.

   S_Link_Static  : aliased constant S := "/STATIC "                       &
                                            "--for-linker=-static";
   --        /NOSTATIC (D)
   --        /STATIC
   --
   --   Indicate to the linker that the link is static.

   S_Link_Subdirs : aliased constant S := "/SUBDIRS=<"                     &
                                             "--subdirs=>";
   --        /SUBDIRS=dir
   --
   --   The actual directories (object, exec, library, ...) are subdirectories
   --   of the directory specified in the project file. If the subdirectory
   --   does not exist, it is created automatically.

   S_Link_Verb    : aliased constant S := "/VERBOSE "                      &
                                            "-v";
   --        /NOVERBOSE (D)
   --        /VERBOSE
   --
   --   Causes additional information to be output, including a full list of
   --   the included object files. This switch option is most useful when you
   --   want to see what set of object files are being used in the link step.

   S_Link_ZZZZZ   : aliased constant S := "/<other> "                      &
                                            "--for-linker=";
   --        /<other>
   --
   --   Any other switch that will be transmitted to the underlying linker.

   Link_Switches : aliased constant Switches :=
                     (S_Link_Add     'Access,
                      S_Link_Bind    'Access,
                      S_Link_Debug   'Access,
                      S_Link_Nodebug 'Access,
                      S_Link_Execut  'Access,
                      S_Link_Ext     'Access,
                      S_Link_Follow  'Access,
                      S_Link_Forlink 'Access,
                      S_Link_Force   'Access,
                      S_Link_Ident   'Access,
                      S_Link_Libdir  'Access,
                      S_Link_Library 'Access,
                      S_Link_Mess    'Access,
                      S_Link_Nocomp  'Access,
                      S_Link_Nofiles 'Access,
                      S_Link_Noinhib 'Access,
                      S_Link_Project 'Access,
                      S_Link_Return  'Access,
                      S_Link_Static  'Access,
                      S_Link_Subdirs 'Access,
                      S_Link_Verb    'Access,
                      S_Link_ZZZZZ   'Access);

   ----------------------------
   -- Switches for GNAT LIST --
   ----------------------------

   S_List_Add     : aliased constant S := "/ADD_PROJECT_SEARCH_DIR=*"      &
                                            "-aP*";
   --        /ADD_PROJECT_SEARCH_PATH=(directory[,...])
   --
   --   Add directories to the project search path.

   S_List_All     : aliased constant S := "/ALL_UNITS "                    &
                                            "-a";
   --        /NOALL_UNITS (D)
   --        /ALL_UNITS
   --
   --   Consider all units, including those of the predefined Ada library.
   --   Especially useful with /DEPENDENCIES.

   S_List_Allproj : aliased constant S := "/ALL_PROJECTS "                 &
                                            "-U";
   --        /NOALL_PROJECTS (D)
   --        /ALL_PROJECTS
   --
   --   When used with a project file and no file specified, indicate
   --   that gnatls should be called for all sources of all projects in
   --   the project tree.

   S_List_Current : aliased constant S := "/CURRENT_DIRECTORY "            &
                                            "!-I-";
   --        /CURRENT_DIRECTORY (D)
   --        /NOCURRENT_DIRECTORY
   --
   --   Look for source, library or object files in the default directory.

   S_List_Depend  : aliased constant S := "/DEPENDENCIES "                 &
                                            "-d";
   --        /NODEPENDENCIES (D)
   --        /DEPENDENCIES

   S_List_Ext     : aliased constant S := "/EXTERNAL_REFERENCE=" & '"'     &
                                            "-X" & '"';
   --        /EXTERNAL_REFERENCE="name=val"
   --
   --   Specifies an external reference to the project manager. Useful only if
   --   /PROJECT_FILE is used.
   --
   --   Example:
   --      /EXTERNAL_REFERENCE="DEBUG=TRUE"

   S_List_Files   : aliased constant S := "/FILES=@"                       &
                                            "-files=@";
   --        /FILES=filename
   --
   --   Take as arguments the files that are listed in the specified
   --   text file.

   S_List_Follow  : aliased constant S := "/FOLLOW_LINKS_FOR_FILES "       &
                                            "-eL";
   --        /NOFOLLOW_LINKS_FOR_FILES (D)
   --        /FOLLOW_LINKS_FOR_FILES
   --
   --    Follow links when parsing project files

   S_List_Mess    : aliased constant S := "/MESSAGES_PROJECT_FILE="        &
                                            "DEFAULT "                     &
                                               "-vP0 "                     &
                                            "MEDIUM "                      &
                                               "-vP1 "                     &
                                            "HIGH "                        &
                                               "-vP2";
   --        /MESSAGES_PROJECT_FILE[=messages-option]
   --
   --   Specifies the "verbosity" of the parsing of project files.
   --   messages-option may be one of the following:
   --
   --      DEFAULT (D)  No messages are output if there is no error or warning.
   --
   --      MEDIUM       A small number of messages are output.
   --
   --      HIGH         A great number of messages are output, most of them not
   --                   being useful for the user.

   S_List_Nostinc : aliased constant S := "/NOSTD_INCLUDES "               &
                                            "-nostdinc";
   --        /NOSTD_INCLUDES
   --
   --   Do not look for sources of the run time in the standard directory.

   S_List_Object  : aliased constant S := "/OBJECT_SEARCH=*"               &
                                            "-aO*";
   --        /OBJECT_SEARCH=(directory,...)
   --
   --   When looking for library and object files look also in the specified
   --   directories.

   S_List_Output  : aliased constant S := "/OUTPUT="                       &
                                            "SOURCES "                     &
                                               "-s "                       &
                                            "DEPEND "                      &
                                               "-d "                       &
                                            "OBJECTS "                     &
                                               "-o "                       &
                                            "UNITS "                       &
                                               "-u "                       &
                                            "OPTIONS "                     &
                                               "-h "                       &
                                            "VERBOSE "                     &
                                               "-v ";
   --        /OUTPUT=(option,option,...)
   --
   --        SOURCES (D)     Only output information about source files.
   --
   --        DEPEND          List sources from which specified units depend on.
   --
   --        OBJECTS         Only output information about object files.
   --
   --        UNITS           Only output information about compilation units.
   --
   --        OPTIONS         Output the list of options.
   --
   --        VERBOSE         Output the complete source and object paths.
   --                        Do not use the default column layout but instead
   --                        use long format giving as much as information
   --                        possible on each requested units, including
   --                        special characteristics.

   S_List_Project : aliased constant S := "/PROJECT_FILE=<"                &
                                            "-P>";
   --        /PROJECT_FILE=filename
   --
   --   Specifies the main project file to be used. The project files rooted
   --   at the main project file will be parsed before doing any listing.
   --   The source and object directories to be searched will be communicated
   --   to gnatlist through logical names ADA_PRJ_INCLUDE_FILE and
   --   ADA_PRJ_OBJECTS_FILE.

   S_List_Search  : aliased constant S := "/SEARCH=*"                      &
                                            "-I*";
   --        /SEARCH=(directory,...)
   --
   --   Search the specified directories for both source and object files.

   S_List_Source  : aliased constant S := "/SOURCE_SEARCH=*"               &
                                            "-aI*";
   --        /SOURCE_SEARCH=(directory,...)
   --
   --   When looking for source files also look in the specified directories.

   S_List_Subdirs : aliased constant S := "/SUBDIRS=<"                     &
                                             "--subdirs=>";
   --        /SUBDIRS=dir
   --
   --   The actual directories (object, exec, library, ...) are subdirectories
   --   of the directory specified in the project file. If the subdirectory
   --   does not exist, it is created automatically.

   List_Switches : aliased constant Switches :=
                     (S_List_Add     'Access,
                      S_List_All     'Access,
                      S_List_Allproj 'Access,
                      S_List_Current 'Access,
                      S_List_Depend  'Access,
                      S_List_Ext     'Access,
                      S_List_Files   'Access,
                      S_List_Follow  'Access,
                      S_List_Mess    'Access,
                      S_List_Nostinc 'Access,
                      S_List_Object  'Access,
                      S_List_Output  'Access,
                      S_List_Project 'Access,
                      S_List_Search  'Access,
                      S_List_Source  'Access,
                      S_List_Subdirs 'Access);

   ----------------------------
   -- Switches for GNAT MAKE --
   ----------------------------

   S_Make_Actions : aliased constant S := "/ACTIONS="                      &
                                            "COMPILE "                     &
                                               "-c "                       &
                                            "BIND "                        &
                                               "-b "                       &
                                            "LINK "                        &
                                               "-l ";
   --        /ACTIONS=(keyword[,...])
   --
   --  GNAT MAKE default behavior is to check if the sources are up to date,
   --  compile those sources that are not up to date, bind the main source,
   --  then link the executable.
   --
   --  With the /ACTIONS qualifier, GNAT MAKE may be restricted to one or
   --  two of these three steps:
   --
   --  o Compile
   --  o Bind
   --  o Link
   --
   --
   --   You may specify one or more of the following keywords to the /ACTIONS
   --   qualifier:
   --
   --   BIND            Bind only. Can be combined with /ACTIONS=COMPILE
   --                   to do compilation and binding, but no linking.
   --                   Can be combined with /ACTIONS=LINK to do binding and
   --                   linking. When not combined with /ACTIONS=COMPILE,
   --                   all the units in the closure of the main program must
   --                   have been previously compiled and must be up to date.
   --
   --   COMPILE         Compile only. Do not perform binding, except when
   --                   /ACTIONS=BIND is also specified. Do not perform
   --                   linking, except if both /ACTIONS=BIND and /ACTIONS=LINK
   --                   are also specified.
   --
   --   LINK            Link only. Can be combined with /ACTIONS=BIND to do
   --                   binding and linking. Linking will not be performed
   --                   if combined with /ACTIONS=COMPILE but not with
   --                   /ACTIONS=BIND\. When not combined with /ACTIONS=BIND
   --                   all the units in the closure of the main program must
   --                   have been previously compiled and must be up to date,
   --                   and the main program need to have been bound.

   S_Make_Add     : aliased constant S := "/ADD_PROJECT_SEARCH_DIR=*"      &
                                            "-aP*";
   --        /ADD_PROJECT_SEARCH_PATH=(directory[,...])
   --
   --   Add directories to the project search path.

   S_Make_All     : aliased constant S := "/ALL_FILES "                    &
                                            "-a";
   --        /NOALL_FILES (D)
   --        /ALL_FILES
   --
   --   Consider all files in the make process, even the GNAT internal system
   --   files (for example, the predefined Ada library files). By default,
   --   GNAT MAKE does not check these files (however, if there is an
   --   installation problem, it will be caught when GNAT MAKE binds your
   --   program). You may have to specify this qualifier if you are working on
   --   GNAT itself.  The vast majority of GNAT MAKE users never need to
   --   specify this switch.  All GNAT internal files with will be compiled
   --   with /STYLE_CHECK=GNAT.

   S_Make_Allproj : aliased constant S := "/ALL_PROJECTS "                 &
                                            "-U";
   --        /NOALL_PROJECTS (D)
   --        /ALL_PROJECTS
   --
   --   Implies /Unique.
   --   When used without project files, it is equivalent to /UNIQUE.
   --   When used with a project file with no main (neither on the command
   --   line nor in the attribute Main) check every source of every project,
   --   recompile all sources that are not up to date and rebuild libraries
   --   if necessary.

   S_Make_Bind    : aliased constant S := "/BINDER_QUALIFIERS=?"           &
                                            "-bargs BIND";
   --        /BINDER_QUALIFIERS
   --
   --   Any qualifiers specified after this qualifier other than
   --   /COMPILER_QUALIFIERS, /LINKER_QUALIFIERS and /MAKE_QUALIFIERS will be
   --   passed to any GNAT BIND commands generated by GNAT MAKE.

   S_Make_Bindprj : aliased constant S := "/BND_LNK_FULL_PROJECT "         &
                                            "-B";
   --        /BND_LNK_FULL_PROJECT
   --
   --   Bind and link all sources of a project, without any consideration
   --   to attribute Main, if there is one. This qualifier need to be
   --   used in conjunction with the /PROJECT_FILE= qualifier and cannot
   --   be used with a main subprogram on the command line or for
   --   a library project file. As the binder is invoked with the option
   --   meaning "No Ada main subprogram", the user must ensure that the
   --   proper options are specified to the linker. This qualifier is
   --   normally used when the main subprogram is in a foreign language
   --   such as C.

   S_Make_Comp    : aliased constant S := "/COMPILER_QUALIFIERS=?"         &
                                            "-cargs COMPILE";
   --        /COMPILER_QUALIFIERS
   --
   --   Any qualifiers specified after this qualifier other than
   --   /BINDER_QUALIFIERS, /LINKER_QUALIFIERS and /MAKE_QUALIFIERS will be
   --   passed to any GNAT COMPILE commands generated by GNAT MAKE.

   S_Make_Cond    : aliased constant S := "/CONDITIONAL_SOURCE_SEARCH=*"   &
                                            "-A*";
   --        /CONDITIONAL_SOURCE_SEARCH=dir
   --
   --   Equivalent to "/SOURCE_SEARCH=dir /SKIP_MISSING=dir".

   S_Make_Cont    : aliased constant S := "/CONTINUE_ON_ERROR "            &
                                            "-k";
   --        /NOCONTINUE_ON_ERROR (D)
   --        /CONTINUE_ON_ERROR
   --
   --   Keep going. Continue as much as possible after a compilation error.
   --   To ease the programmer's task in case of compilation errors, the list
   --   of sources for which the compile fails is given when GNAT MAKE
   --   terminates.

   S_Make_Current : aliased constant S := "/CURRENT_DIRECTORY "            &
                                            "!-I-";
   --        /CURRENT_DIRECTORY (D)
   --        /NOCURRENT_DIRECTORY
   --
   --   Look for source, library or object files in the default directory.

   S_Make_Dep     : aliased constant S := "/DEPENDENCIES_LIST "            &
                                            "-M";
   --        /NODEPENDENCIES_LIST (D)
   --        /DEPENDENCIES_LIST
   --
   --   Check if all objects are up to date. If they are, output the object
   --   dependences to SYS$OUTPUT in a form that can be directly exploited in
   --   a Unix-style Makefile. By default, each source file is prefixed with
   --   its (relative or absolute) directory name. This name is whatever you
   --   specified in the various /SOURCE_SEARCH and /SEARCH qualifiers.  If
   --   you also specify the /QUIET qualifier, only the source file names,
   --   without relative paths, are output. If you just specify the
   --   /DEPENDENCY_LIST qualifier, dependencies of the GNAT internal system
   --   files are omitted.  This is typically what you want. If you also
   --   specify the /ALL_FILES qualifier, dependencies of the GNAT internal
   --   files are also listed. Note that dependencies of the objects in
   --   external Ada libraries (see the /SKIP_MISSING qualifier) are never
   --   reported.

   S_Make_Dirobj  : aliased constant S := "/DIRECTORY_OBJECTS=@"           &
                                            "-D@";
   --        /DIRECTORY_OBJECTS=<file>
   --
   --   Put all object files and .ALI files in <file>.
   --   This qualifier is not compatible with /PROJECT_FILE.

   S_Make_Disprog : aliased constant S := "/DISPLAY_PROGRESS "             &
                                            "-d";
   --        /NOPLAY_PROGRESS (D)
   --        /DISPLAY_PROGRESS
   --
   --   Display progress for each source, up to date or not, as a single line
   --      completed x out of y (zz%)
   --   If the file needs to be compiled this is displayed after the
   --   invocation of the compiler. These lines are displayed even in quiet
   --   output mode (/QUIET).

   S_Make_Doobj   : aliased constant S := "/DO_OBJECT_CHECK "              &
                                            "-n";
   --        /NODO_OBJECT_CHECK (D)
   --        /DO_OBJECT_CHECK
   --
   --   Don't compile, bind, or link. Output a single command that will
   --   recompile an out of date unit, if any. Repeated use of this option,
   --   followed by carrying out the indicated compilation, will eventually
   --   result in recompiling all required units.
   --
   --   If any ALI is missing during the process, GNAT MAKE halts and
   --   displays an error message.

   S_Make_Execut  : aliased constant S := "/EXECUTABLE=@"                  &
                                            "-o@";
   --        /EXECUTABLE=exec-name
   --
   --   The name of the final executable program will be 'exec_name'. If this
   --   qualifier is omitted the default name for the executable will be the
   --   name of the input file with an EXE filetype.  You may prefix
   --   'exec_name' with a relative or absolute directory path.

   S_Make_Ext     : aliased constant S := "/EXTERNAL_REFERENCE=" & '"'     &
                                            "-X" & '"';
   --        /EXTERNAL_REFERENCE="name=val"
   --
   --   Specifies an external reference to the project manager. Useful only if
   --   /PROJECT_FILE is used.
   --
   --   Example:
   --      /EXTERNAL_REFERENCE="DEBUG=TRUE"

   S_Make_Follow  : aliased constant S := "/FOLLOW_LINKS_FOR_FILES "       &
                                            "-eL";
   --        /NOFOLLOW_LINKS_FOR_FILES (D)
   --        /FOLLOW_LINKS_FOR_FILES
   --
   --    Follow links when parsing project files

   S_Make_Force   : aliased constant S := "/FORCE_COMPILE "                &
                                            "-f";
   --        /NOFORCE_COMPILE (D)
   --        /FORCE_COMPILE
   --
   --   Force recompilations. Recompile all sources, even though some object
   --   files may be up to date, but don't recompile predefined or GNAT
   --   internal files unless the /ALL_FILES qualifier is also specified.

   S_Make_Full    : aliased constant S := "/FULL_PATH_IN_BRIEF_MESSAGES "  &
                                            "-F";
   --        /NOFULL_PATH_IN_BRIEF_MESSAGES (D)
   --        /FULL_PATH_IN_BRIEF_MESSAGES
   --
   --   When using project files, if some errors or warnings are detected
   --   during parsing and verbose mode is not in effect (no use of qualifier
   --   /VERBOSE), then error lines start with the full path name of the
   --   project file, rather than its simple file name.

   S_Make_Hi_Verb : aliased constant S := "/HIGH_VERBOSITY "               &
                                          "-vh";
   --        /NOHIGH_VERBOSITY (D)
   --        /HIGH_VERBOSITY
   --
   --   Displays the reason for all recompilations GNAT MAKE decides are
   --   necessary, in high verbosity. Equivalent to /VERBOSE.

   S_Make_Inplace : aliased constant S := "/IN_PLACE "                     &
                                            "-i";
   --        /NOIN_PLACE (D)
   --        /IN_PLACE
   --
   --   In normal mode, GNAT MAKE compiles all object files and ALI files
   --   into the current directory. If the /IN_PLACE switch is used,
   --   then instead object files and ALI files that already exist are over-
   --   written in place. This means that once a large project is organized
   --   into separate directories in the desired manner, then GNAT MAKE will
   --   automatically maintain and update this organization. If no ALI files
   --   are found on the Ada object path, the new object and ALI files are
   --   created in the directory containing the source being compiled.

   S_Make_Index   : aliased constant S := "/SOURCE_INDEX=#"                &
                                             "-eI#";
   --        /SOURCE_INDEX=nnn
   --
   --   Specifies the index of the units in the source file
   --   By default, source files are mono-unit and there is no index
   --   When /SOURCE_INDEX=nnn is specified, only one main may be specified
   --   on the command line.

   S_Make_Library : aliased constant S := "/LIBRARY_SEARCH=*"              &
                                            "-L*";
   --        /LIBRARY_SEARCH=(directory[,...])
   --
   --   Add the specified directories to the list of directories in which the
   --   linker will search for libraries.

   S_Make_Link    : aliased constant S := "/LINKER_QUALIFIERS=?"           &
                                            "-largs LINK";
   --        /LINKER_QUALIFIERS
   --
   --   Any qualifiers specified after this qualifier other than
   --   /COMPILER_QUALIFIERS, /BINDER_QUALIFIERS and /MAKE_QUALIFIERS will be
   --   passed to any GNAT LINK commands generated by GNAT LINK.

   S_Make_Low_Verb : aliased constant S := "/LOW_VERBOSITY "               &
                                           "-vl";
   --        /NOLOW_VERBOSITY (D)
   --        /LOW_VERBOSITY
   --
   --   Displays the reason for all recompilations GNAT MAKE decides are
   --   necessary, in low verbosity, that is with less output than
   --   /MEDIUM_VERBOSITY, /HIGH_VERBOSITY or /VERBOSE.

   S_Make_Make    : aliased constant S := "/MAKE_QUALIFIERS=?"             &
                                            "-margs MAKE";
   --        /MAKE_QUALIFIERS
   --
   --   Any qualifiers specified after this qualifier other than
   --   /COMPILER_QUALIFIERS, /BINDER_QUALIFIERS and /LINKER_QUALIFIERS
   --   are for the benefit of GNAT MAKE itself.

   S_Make_Mapping : aliased constant S := "/MAPPING "                      &
                                            "-C";
   --        /NOMAPPING (D)
   --        /MAPPING
   --
   --   Use a mapping file.  A mapping file is a way to communicate to the
   --   compiler two mappings: from unit names to file names (without any
   --   directory information) and from file names to path names (with full
   --   directory information). These mappings are used by the compiler to
   --   short-circuit the path search. When GNAT MAKE is invoked with this
   --   qualifier, it will create a mapping file, initially populated by the
   --   project manager, if /PROJECT_File= is used, otherwise initially empty.
   --   Each invocation of the compiler will add the newly accessed sources to
   --   the mapping file. This will improve the source search during the next
   --   invocations of the compiler

   S_Make_Med_Verb : aliased constant S := "/MEDIUM_VERBOSITY "            &
                                           "-vm";
   --        /NOMEDIUM_VERBOSITY (D)
   --        /MEDIUM_VERBOSITY
   --
   --   Displays the reason for all recompilations GNAT MAKE decides are
   --   necessary, in medium verbosity, that is with potentially less output
   --   than /HIGH_VERBOSITY or /VERBOSE.

   S_Make_Mess    : aliased constant S := "/MESSAGES_PROJECT_FILE="        &
                                            "DEFAULT "                     &
                                               "-vP0 "                     &
                                            "MEDIUM "                      &
                                               "-vP1 "                     &
                                            "HIGH "                        &
                                               "-vP2";
   --        /MESSAGES_PROJECT_FILE[=messages-option]
   --
   --   Specifies the "verbosity" of the parsing of project files.
   --   messages-option may be one of the following:
   --
   --      DEFAULT (D)  No messages are output if there is no error or warning.
   --
   --      MEDIUM       A small number of messages are output.
   --
   --      HIGH         A great number of messages are output, most of them not
   --                   being useful for the user.

   S_Make_Minimal : aliased constant S := "/MINIMAL_RECOMPILATION "        &
                                            "-m";
   --        /NOMINIMAL_RECOMPILATION (D)
   --        /MINIMAL_RECOMPILATION
   --
   --   Specifies that the minimum necessary amount of recompilation
   --   be performed. In this mode GNAT MAKE ignores time stamp differences
   --   when the only modifications to a source file consist in
   --   adding/removing comments, empty lines, spaces or tabs.

   S_Make_Missing : aliased constant S := "/CREATE_MISSING_DIRS "          &
                                            "-p";
   --        /NOCREATE_MISSING_DIRS (D)
   --        /CREATE_MISSING_DIRS
   --
   --   When an object directory, a library directory or an exec directory
   --   in missing, attempt to create the directory.

   S_Make_Nolink  : aliased constant S := "/NOLINK "                       &
                                            "-c";
   --        /NOLINK
   --
   --   Compile only. Do not perform binding and linking. If the root unit is
   --   not a main unit, this is the default.  Otherwise GNAT MAKE will
   --   attempt binding and linking unless all objects are up to date and the
   --   executable is more recent than the objects.
   --   This is equivalent to /ACTIONS=COMPILE

   S_Make_Nomain  : aliased constant S := "/NOMAIN "                       &
                                            "-z";
   --        /NOMAIN
   --
   --   No main subprogram. Bind and link the program even if the unit name
   --   given on the command line is a package name. The resulting executable
   --   will execute the elaboration routines of the package and its closure,
   --   then the finalization routines.

   S_Make_Nonpro  : aliased constant S := "/NON_PROJECT_UNIT_COMPILATION " &
                                            "-x";
   --        /NON_PROJECT_UNIT_COMPILATION
   --
   --    Normally, when using project files, a unit that is not part of any
   --    project file, cannot be compile. These units may be compile, when
   --    needed, if this qualifier is specified.

   S_Make_Nostinc : aliased constant S := "/NOSTD_INCLUDES "               &
                                            "-nostdinc";
   --        /NOSTD_INCLUDES
   --
   --    Do not look for sources the in the system default directory.

   S_Make_Nostlib : aliased constant S := "/NOSTD_LIBRARIES "              &
                                            "-nostdlib";
   --        /NOSTD_LIBRARIES
   --
   --    Do not look for library files in the system default directory.

   S_Make_Object  : aliased constant S := "/OBJECT_SEARCH=*"               &
                                            "-aO*";
   --        /OBJECT_SEARCH=(directory[,...])
   --
   --   When looking for library and object files look also in the specified
   --   directories.

   S_Make_Proc    : aliased constant S := "/PROCESSES=#"                   &
                                            "-j#";
   --        /NOPROCESSES (D)
   --        /PROCESSES=NNN
   --
   --   Use NNN processes to carry out the (re)compilations. If you have a
   --   multiprocessor machine, compilations will occur in parallel.  In the
   --   event of compilation errors, messages from various compilations might
   --   get interspersed (but GNAT MAKE will give you the full ordered list of
   --   failing compiles at the end). This can at times be annoying.  To get a
   --   clean list of error messages don't use this qualifier.

   S_Make_Nojobs  : aliased constant S := "/NOPROCESSES "                  &
                                            "-j1";
   --  NODOC (see /PROCESS)

   S_Make_Project : aliased constant S := "/PROJECT_FILE=<"                &
                                            "-P>";
   --        /PROJECT_FILE=filename
   --
   --   Specifies the main project file to be used. The project files rooted
   --   at the main project file will be parsed before any other processing to
   --   set the building environment.

   S_Make_Quiet   : aliased constant S := "/QUIET "                        &
                                            "-q";
   --        /NOQUIET (D)
   --        /QUIET
   --
   --   When this qualifiers is specified, the commands carried out by GNAT
   --   MAKE are not displayed.

   S_Make_Reason  : aliased constant S := "/REASONS "                      &
                                            "-v";
   --        /NOREASONS (D)
   --        /REASONS
   --
   --   Displays the reason for all recompilations GNAT MAKE decides are
   --   necessary.

   S_Make_RTS     : aliased constant S := "/RUNTIME_SYSTEM=|"              &
                                            "--RTS=|";
   --        /RUNTIME_SYSTEM=xxx
   --
   --    Build against an alternate runtime system named xxx or RTS-xxx.

   S_Make_Search  : aliased constant S := "/SEARCH=*"                      &
                                            "-I*";
   --        /SEARCH=(directory[,...])
   --
   --   Search the specified directories for both source and object files.

   S_Make_Single  : aliased constant S := "/SINGLE_COMPILE_PER_OBJ_DIR "   &
                                            "--single-compile-per-obj-dir";
   --        /NOSINGLE_COMPILE_PER_OBJ_DIR (D)
   --        /SINGLE_COMPILE_PER_OBJ_DIR
   --
   --    When project files are used, do not allow simultaneous compilations
   --    for the same object directory.

   S_Make_Skip    : aliased constant S := "/SKIP_MISSING=*"                &
                                            "-aL*";
   --        /SKIP_MISSING=(directory[,...])
   --
   --   Skip missing library sources if ALI in 'directory'.

   S_Make_Source  : aliased constant S := "/SOURCE_SEARCH=*"               &
                                            "-aI*";
   --        /SOURCE_SEARCH=(directory[,...])
   --
   --   When looking for source files also look in the specified directories.

   S_Make_Src_Info : aliased constant S := "/SRC_INFO=<"                   &
                                             "--source-info=>";
   --        /SRC_INFO=source-info-file
   --
   --   Specify a source info file to be read or written by the Project
   --   Manager when project files are used.

   S_Make_Stand   : aliased constant S := "/STANDARD_OUTPUT_FOR_COMMANDS " &
                                            "-eS";
   --        /NOSTANDARD_OUTPUT_FOR_COMMANDS (D)
   --        /STANDARD_OUTPUT_FOR_COMMANDS
   --
   --   Output the commands for the compiler, the binder and the linker
   --   on SYS$OUTPUT, instead of SYS$ERROR.

   S_Make_Subdirs : aliased constant S := "/SUBDIRS=<"                     &
                                             "--subdirs=>";
   --        /SUBDIRS=dir
   --
   --   The actual directories (object, exec, library, ...) are subdirectories
   --   of the directory specified in the project file. If the subdirectory
   --   does not exist, it is created automatically.

   S_Make_Switch  : aliased constant S := "/SWITCH_CHECK "                 &
                                            "-s";
   --        /NOSWITCH_CHECK (D)
   --        /SWITCH_CHECK
   --
   --   Recompile if compiler switches have changed since last compilation.
   --   All compiler switches but -I and -o are taken into account in the
   --   following way: orders between different "first letter" switches are
   --   ignored, but orders between same switches are taken into account.
   --   For example, -O -O2 is different than -O2 -O, but -g -O is equivalent
   --   to -O -g.

   S_Make_USL : aliased constant S := "/UNCHECKED_SHARED_LIB_IMPORTS " &
                                         "--unchecked-shared-lib-imports";
   --        /NOUNCHECKED_SHARED_LIB_IMPORTS (D)
   --        /UNCHECKED_SHARED_LIB_IMPORTS
   --
   --   Allow shared library projects to import static library projects

   S_Make_Unique  : aliased constant S := "/UNIQUE "                       &
                                            "-u";
   --        /NOUNIQUE (D)
   --        /UNIQUE
   --
   --  Recompile at most the main file. It implies /ACTIONS=COMPILE.
   --  Combined with /FORCE_COMPILE, it is equivalent to calling the compiler
   --  directly.

   S_Make_Use_Map : aliased constant S := "/USE_MAPPING_File=@"            &
                                            "-C=@";
   --        /USE_MAPPING_FILE=file_name
   --
   --   Use a specific mapping file. The file 'file_name', specified as a path
   --   name (absolute or relative) by this qualifier, should already exist,
   --   otherwise the qualifier is ineffective. The specified mapping file
   --   will be communicated to the compiler. This switch is not compatible
   --   with a project file (/PROJECT_FILE=) or with multiple compiling
   --   processes (/PROCESSES=nnn, when nnn is greater than 1).

   S_Make_Verbose : aliased constant S := "/VERBOSE "                      &
                                            "-v";
   --        /NOVERBOSE (D)
   --        /VERBOSE
   --
   --   Displays the reason for all recompilations GNAT MAKE decides are
   --   necessary.

   Make_Switches : aliased constant Switches :=
                     (S_Make_Add     'Access,
                      S_Make_Actions 'Access,
                      S_Make_All     'Access,
                      S_Make_Allproj 'Access,
                      S_Make_Bind    'Access,
                      S_Make_Comp    'Access,
                      S_Make_Cond    'Access,
                      S_Make_Cont    'Access,
                      S_Make_Current 'Access,
                      S_Make_Dep     'Access,
                      S_Make_Dirobj  'Access,
                      S_Make_Disprog 'Access,
                      S_Make_Doobj   'Access,
                      S_Make_Execut  'Access,
                      S_Make_Ext     'Access,
                      S_Make_Follow  'Access,
                      S_Make_Force   'Access,
                      S_Make_Full    'Access,
                      S_Make_Hi_Verb 'Access,
                      S_Make_Inplace 'Access,
                      S_Make_Index   'Access,
                      S_Make_Library 'Access,
                      S_Make_Link    'Access,
                      S_Make_Low_Verb'Access,
                      S_Make_Make    'Access,
                      S_Make_Mapping 'Access,
                      S_Make_Med_Verb'Access,
                      S_Make_Mess    'Access,
                      S_Make_Minimal 'Access,
                      S_Make_Missing 'Access,
                      S_Make_Nolink  'Access,
                      S_Make_Nomain  'Access,
                      S_Make_Nonpro  'Access,
                      S_Make_Nostinc 'Access,
                      S_Make_Nostlib 'Access,
                      S_Make_Object  'Access,
                      S_Make_Proc    'Access,
                      S_Make_Nojobs  'Access,
                      S_Make_Project 'Access,
                      S_Make_Quiet   'Access,
                      S_Make_Reason  'Access,
                      S_Make_RTS     'Access,
                      S_Make_Search  'Access,
                      S_Make_Single  'Access,
                      S_Make_Skip    'Access,
                      S_Make_Source  'Access,
                      S_Make_Src_Info'Access,
                      S_Make_Stand   'Access,
                      S_Make_Subdirs 'Access,
                      S_Make_Switch  'Access,
                      S_Make_USL     'Access,
                      S_Make_Unique  'Access,
                      S_Make_Use_Map 'Access,
                      S_Make_Verbose 'Access);

   ------------------------------
   -- Switches for GNAT METRIC --
   ------------------------------

   S_Metric_Add     : aliased constant S := "/ADD_PROJECT_SEARCH_DIR=*"    &
                                              "-aP*";
   --        /ADD_PROJECT_SEARCH_PATH=(directory[,...])
   --
   --   Add directories to the project search path.

   S_Metric_All_Prjs : aliased constant S := "/ALL_PROJECTS "              &
                                               "-U";
   --        /NOALL_PROJECTS (D)
   --        /ALL_PROJECTS
   --   When GNAT METRIC is used with a Project File and no source is
   --   specified, the underlying tool gnatmetric is called for all the
   --   sources of all the Project Files in the project tree.

   S_Metric_Debug    : aliased constant S := "/DEBUG_OUTPUT "              &
                                             "-dv";
   --      /DEBUG_OUTPUT
   --
   --   Generate the debug information

   S_Metric_Direct   : aliased constant S := "/DIRECTORY=@"                &
                                             "-d=@";
   --      /DIRECTORY=pathname
   --
   --   Put the files with detailed metric information into the specified
   --   directory

   S_Metric_Element : aliased constant S := "/ELEMENT_METRICS="            &
                                             "ALL "                        &
                                              "!-ed,!-es,!-enl,!-eps,"     &
                                              "!-eas,!-ept,!-eat,!-enu,"   &
                                              "!-ec "                      &
                                             "DECLARATION_TOTAL "          &
                                              "-ed "                       &
                                             "STATEMENT_TOTAL "            &
                                              "-es "                       &
                                             "LOOP_NESTING_MAX "           &
                                              "-enl "                      &
                                             "INT_SUBPROGRAMS "            &
                                              "-eps "                      &
                                             "SUBPROGRAMS_ALL "            &
                                              "-eas "                      &
                                             "INT_TYPES "                  &
                                              "-ept "                      &
                                             "TYPES_ALL "                  &
                                              "-eat "                      &
                                             "PROGRAM_NESTING_MAX "        &
                                              "-enu "                      &
                                             "CONSTRUCT_NESTING_MAX "      &
                                              "-ec";
   --  NODOC  (see /SYNTAX_METRICS)

   S_Metric_Syntax : aliased constant S := "/SYNTAX_METRICS="              &
                                             "ALL "                        &
                                             "--syntax-all "               &
                                             "NONE "                       &
                                             "--no-syntax-all "            &
                                             "DECLARATIONS "               &
                                             "--declarations "             &
                                             "NODECLARATIONS "             &
                                             "--no-declarations "          &
                                             "STATEMENTS "                 &
                                             "--statements "               &
                                             "NOSTATEMENTS "               &
                                             "--no-statements "            &
                                             "PUBLIC_SUBPROGRAMS "         &
                                             "--public-subprograms "       &
                                             "NOPUBLIC_SUBPROGRAMS "       &
                                             "--no-public-subprograms "    &
                                             "ALL_SUBPROGRAMS "            &
                                             "--all-subprograms "          &
                                             "NOALL_SUBPROGRAMS "          &
                                             "--no-all-subprograms "       &
                                             "PUBLIC_TYPES "               &
                                             "--public-types "             &
                                             "NOPUBLIC_TYPES "             &
                                             "--no-public-types "          &
                                             "ALL_TYPES "                  &
                                             "--all-types "                &
                                             "NOALL_TYPES "                &
                                             "--no-all-types "             &
                                             "UNIT_NESTING "               &
                                             "--unit-nesting "             &
                                             "NOUNIT_NESTING "             &
                                             "--no-unit-nesting "          &
                                             "CONSTRUCT_NESTING "          &
                                             "--construct-nesting "        &
                                             "NOCONSTRUCT_NESTING "        &
                                             "--no-construct-nesting";
   --       /SYNTAX_METRICS(option, option ...)
   --
   --   Specifies the syntax element metrics to be computed (if at least one
   --   positive syntax element metric, line metric, complexity or coupling
   --   metric is specified then only explicitly specified syntax element
   --   metrics are computed and reported)
   --
   --   option may be one of the following:
   --
   --     ALL (D)               All the syntax element metrics are computed
   --     NONE                  None of syntax element metrics is computed
   --     DECLARATIONS          Compute the total number of declarations
   --     NODECLARATIONS        Do not compute the total number of declarations
   --     STATEMENTS            Compute the total number of statements
   --     NOSTATEMENTS          Do not compute the total number of statements
   --     PUBLIC_SUBPROGRAMS    Compute the number of public subprograms
   --     NOPUBLIC_SUBPROGRAMS  Do not compute the number of public subprograms
   --     ALL_SUBPROGRAMS       Compute the number of all the subprograms
   --     NOALL_SUBPROGRAMS     Do not compute the number of all the
   --                           subprograms
   --     PUBLIC_TYPES          Compute the number of public types
   --     NOPUBLIC_TYPES        Do not compute the number of public types
   --     ALL_TYPES             Compute the number of all the types
   --     NOALL_TYPES           Do not compute the number of all the types
   --     UNIT_NESTING          Compute the maximal program unit nesting
   --                           level
   --     NOUNIT_NESTING        Do not compute the maximal program unit
   --                           nesting level
   --     CONSTRUCT_NESTING     Compute the maximal construct nesting level
   --     NOCONSTRUCT_NESTING   Do not compute the maximal construct nesting
   --                           level
   --
   --   All combinations of syntax element metrics options are allowed.

   S_Metric_Ext     : aliased constant S := "/EXTERNAL_REFERENCE=" & '"'   &
                                             "-X" & '"';
   --       /EXTERNAL_REFERENCE="name=val"
   --
   --   Specifies an external reference to the project manager. Useful only if
   --   /PROJECT_FILE is used.
   --
   --   Example:
   --      /EXTERNAL_REFERENCE="DEBUG=TRUE"

   S_Metric_Files   : aliased constant S := "/FILES=@"                     &
                                             "-files=@";
   --      /FILES=filename
   --
   --   Take as arguments the files that are listed in the specified
   --   text file.

   S_Metric_Format  : aliased constant S := "/FORMAT_OUTPUT="              &
                                             "DEFAULT "                    &
                                              "!-x,!-nt,!-sfn "            &
                                             "XML "                        &
                                              "-x "                        &
                                             "XSD "                        &
                                              "-xs "                       &
                                             "NO_TEXT "                    &
                                              "-nt "                       &
                                             "SHORT_SOURCE_FILE_NAME "     &
                                              "-sfn";
   --       /FORMAT_OUTPUT=(option, option ...)
   --
   --   Specifies the details of the tool output
   --
   --   option may be one of the following:
   --
   --     DEFAULT (D)             Generate the text output only, use full
   --                             argument source names in global information
   --     XML                     Generate the output in XML format
   --     XSD                     Generate the output in XML format, and
   --                             generate an XML schema file that describes
   --                             the structure of XML metrics report
   --     NO_TEXT                 Do not generate the text output (implies XML)
   --     SHORT_SOURCE_FILE_NAME  Use short argument source names in output

   S_Metric_Globout : aliased constant S := "/GLOBAL_OUTPUT=@"             &
                                             "-og@";
   --        /GLOBAL_OUTPUT=filename
   --
   --   Put the textual global metric information into the specified file

   S_Metric_Line     : aliased constant S := "/LINE_METRICS="              &
                                                "ALL "                     &
                                                 "!-la,!-lcode,!-lcomm,"   &
                                                 "!-leol,!-lb "            &
                                                "LINES_ALL "               &
                                                 "-la "                    &
                                                "CODE_LINES "              &
                                                 "-lcode "                 &
                                                "COMENT_LINES "            &
                                                 "-lcomm "                 &
                                                "MIXED_CODE_COMMENTS "     &
                                                 "-leol "                  &
                                                "COMMENT_PERCENTAGE "      &
                                                 "-lratio "                &
                                                "BLANK_LINES "             &
                                                 "-lb "                    &
                                                "AVERAGE_LINES_IN_BODIES " &
                                                 "-lav ";
   --  NODOC  (see /LINE_COUNT_METRICS)

   S_Metric_Lines : aliased constant S := "/LINE_COUNT_METRICS="           &
                                           "ALL "                          &
                                           "--lines-all "                  &
                                           "NONE "                         &
                                           "--no-lines-all "               &
                                           "ALL_LINES "                    &
                                           "--lines "                      &
                                           "NOALL_LINES "                  &
                                           "--no-lines "                   &
                                           "CODE_LINES "                   &
                                           "--lines-code "                 &
                                           "NOCODE_LINES "                 &
                                           "--no-lines-code "              &
                                           "COMMENT_LINES "                &
                                           "--lines-comment "              &
                                           "NOCOMMENT_LINES "              &
                                           "--no-lines-comment "           &
                                           "CODE_COMMENT_LINES "           &
                                           "--lines-eol-comment "          &
                                           "NOCODE_COMMENT_LINES "         &
                                           "--no-lines-eol-comment "       &
                                           "COMMENT_PERCENTAGE "           &
                                           "--lines-ratio "                &
                                           "NOCOMMENT_PERCENTAGE "         &
                                           "--no-lines-ratio "             &
                                           "BLANK_LINES "                  &
                                           "--lines-blank "                &
                                           "NOBLANK_LINES "                &
                                           "--no-lines-blank "             &
                                           "AVERAGE_BODY_LINES "           &
                                           "--lines-average "              &
                                           "NOAVERAGE_BODY_LINES "         &
                                           "--no-lines-average";
   --      /LINE_COUNT_METRICS=(option, option ...)

   --   Specifies the line metrics to be computed (if at least one positive
   --   syntax element metric, line metric, complexity or coupling metric is
   --   specified then only explicitly specified line metrics are computed
   --   and reported)
   --
   --   option may be one of the following:
   --
   --     ALL (D)               All the line metrics are computed
   --     NONE                  None of line metrics is computed
   --     ALL_LINES             All lines are computed
   --     NOALL_LINES           All lines are not computed
   --     CODE_LINES            Lines with Ada code are computed
   --     NOCODE_LINES          Lines with Ada code are not computed
   --     COMMENT_LINES         Comment lines are computed
   --     NOCOMMENT_LINES       Comment lines are not computed
   --     CODE_COMMENT_LINES    Lines containing both code and comment parts
   --                           are computed
   --     NOCODE_COMMENT_LINES  Lines containing both code and comment parts
   --                           are not computed
   --     COMMENT_PERCENTAGE    Ratio between comment lines and all the lines
   --                           containing comments and program code is
   --                           computed
   --     NOCOMMENT_PERCENTAGE  Ratio between comment lines and all the lines
   --                           containing comments and program code is not
   --                           computed
   --     BLANK_LINES           Blank lines are computed
   --     NOBLANK_LINES         Blank lines are not computed
   --     AVERAGE_BODY_LINES    Average number of code lines in subprogram,
   --                           task and entry bodies and statement sequences
   --                           of package bodies is computed
   --     NOAVERAGE_BODY_LINES  Average number of code lines in subprogram,
   --                           task and entry bodies and statement sequences
   --                           of package bodies is not computed
   --
   --   All combinations of line metrics options are allowed.

   S_Metric_Complexity : aliased constant S := "/COMPLEXITY_METRICS="      &
                                               "ALL "                      &
                                               "--complexity-all "         &
                                              "NONE "                      &
                                              "--no-complexity-all "       &
                                              "CYCLOMATIC "                &
                                              "--complexity-cyclomatic "   &
                                              "NOCYCLOMATIC "              &
                                              "--no-complexity-cyclomatic "&
                                              "ESSENTIAL "                 &
                                              "--complexity-essential "    &
                                              "NOESSENTIAL "               &
                                              "--no-complexity-essential " &
                                              "LOOP_NESTING "              &
                                              "--loop-nesting "            &
                                              "NOLOOP_NESTING "            &
                                              "--no-loop-nesting "         &
                                              "AVERAGE_COMPLEXITY "        &
                                              "--complexity-average "      &
                                              "NOAVERAGE_COMPLEXITY "      &
                                              "--no-complexity-average "   &
                                              "EXTRA_EXIT_POINTS "         &
                                              "--extra-exit-points "       &
                                              "NOEXTRA_EXIT_POINTS "       &
                                              "--no-extra-exit-points";
   --      /COMPLEXITY_METRICS=(option, option ...)

   --   Specifies the complexity metrics to be computed (if at least one
   --   positive syntax element metric, line metric, complexity or coupling
   --   metric is specified then only explicitly specified complexity metrics
   --   are computed and reported)
   --
   --   option may be one of the following:
   --
   --     ALL (D)               All the complexity metrics are computed
   --     NONE                  None of complexity metrics is computed
   --     CYCLOMATIC            Compute the McCabe Cyclomatic Complexity
   --     NOCYCLOMATIC          Do not compute the McCabe Cyclomatic Complexity
   --     ESSENTIAL             Compute the Essential Complexity
   --     NOESSENTIAL           Do not compute the Essential Complexity
   --     LOOP_NESTING          Compute the maximal loop nesting
   --     NOLOOP_NESTING        Do not compute the maximal loop nesting
   --     AVERAGE_COMPLEXITY    Compute the average complexity for executable
   --                           bodies
   --     NOAVERAGE_COMPLEXITY  Do not compute the average complexity for
   --                           executable bodies
   --     EXTRA_EXIT_POINTS     Compute extra exit points metric
   --     NOEXTRA_EXIT_POINTS   Do not compute extra exit points metric
   --
   --   All combinations of line metrics options are allowed.

   S_Metric_Coupling : aliased constant S := "/COUPLING_METRICS="             &
                                           "ALL "                             &
                                           "--coupling-all "                  &
                                           "TAGGED_OUT "                      &
                                           "--tagged-coupling-out "           &
                                           "TAGGED_IN "                       &
                                           "--tagged-coupling-in "            &
                                           "HIERARCHY_OUT "                   &
                                           "--hierarchy-coupling-out "        &
                                           "HIERARCHY_IN "                    &
                                           "--hierarchy-coupling-in "         &
                                           "UNIT_OUT "                        &
                                           "--unit-coupling-out "             &
                                           "UNIT_IN "                         &
                                           "--unit-coupling-in "              &
                                           "CONTROL_OUT "                     &
                                           "--control-coupling-out "          &
                                           "CONTROL_IN "                      &
                                           "--control-coupling-in";

   --      /COUPLING_METRICS=(option, option ...)

   --   Specifies the coupling metrics to be computed.
   --
   --   option may be one of the following:
   --
   --     ALL            All the coupling metrics are computed
   --     NOALL (D)      None of coupling metrics is computed
   --     TAGGED_OUT     Compute tagged (class) far-out coupling
   --     TAGGED_IN      Compute tagged (class) far-in coupling
   --     HIERARCHY_OUT  Compute hieraqrchy (category) far-out coupling
   --     HIERARCHY_IN   Compute hieraqrchy (category) far-in coupling
   --     UNIT_OUT       Compute unit far-out coupling
   --     UNIT_IN        Compute unit far-in coupling
   --     CONTROL_OUT    Compute control far-out coupling
   --     CONTROL_IN     Compute control far-in coupling

   --
   --   All combinations of coupling metrics options are allowed.

   S_Metric_Follow : aliased constant S := "/FOLLOW_LINKS_FOR_FILES "      &
                                             "-eL";
   --        /NOFOLLOW_LINKS_FOR_FILES (D)
   --        /FOLLOW_LINKS_FOR_FILES
   --
   --    Follow links when parsing project files

   S_Metric_No_Local : aliased constant S := "/NO_LOCAL_DETAILS "          &
                                             "-nolocal";
   --        /LOCAL_DETAILS (D)
   --        /NO_LOCAL_DETAILS
   --
   --   Do not compute the detailed metrics for local program units.

   S_Metric_No_Exits_As_Gotos : aliased constant S := "/NO_EXITS_AS_GOTOS " &
                                                      "-ne";
   --        /EXITS_AS_GOTOS (D)
   --        /NO_EXITS_AS_GOTOS
   --
   --   Do not count EXIT statements as GOTOs when computing the Essential
   --   Complexity.

   S_Metric_No_Static_Loop : aliased constant S := "/NO_STATIC_LOOP " &
                                                   "--no-static-loop";
   --        /STATIC_LOOP (D)
   --        /NO_STATIC_LOOP
   --
   --   Do not count static FOR loop statements when computing the Cyclomatic
   --   Complexity.

   S_Metric_Mess    : aliased constant S := "/MESSAGES_PROJECT_FILE="      &
                                             "DEFAULT "                    &
                                                "-vP0 "                    &
                                             "MEDIUM "                     &
                                                "-vP1 "                    &
                                             "HIGH "                       &
                                                "-vP2";
   --        /MESSAGES_PROJECT_FILE[=messages-option]
   --
   --   Specifies the "verbosity" of the parsing of project files.
   --   messages-option may be one of the following:
   --
   --      DEFAULT (D)  No messages are output if there is no error or warning.
   --
   --      MEDIUM       A small number of messages are output.
   --
   --      HIGH         A great number of messages are output, most of them not
   --                   being useful for the user.

   S_Metric_Project : aliased constant S := "/PROJECT_FILE=<"              &
                                             "-P>";
   --        /PROJECT_FILE=filename
   --
   --   Specifies the main project file to be used. The project files rooted
   --   at the main project file will be parsed before the invocation of the
   --   binder.

   S_Metric_Quiet    : aliased constant S := "/QUIET "                     &
                                             "-q";
   --        /NOQUIET (D)
   --        /QUIET
   --
   --   Quiet mode: by default GNAT METRIC outputs to the standard error stream
   --   the number of program units left to be processed. This option turns
   --   this trace off.

   S_Metric_Subdirs : aliased constant S := "/SUBDIRS=<"                   &
                                               "--subdirs=>";
   --        /SUBDIRS=dir
   --
   --   The actual directories (object, exec, library, ...) are subdirectories
   --   of the directory specified in the project file. If the subdirectory
   --   does not exist, it is created automatically.

   S_Metric_Suffix  : aliased constant S := "/SUFFIX_DETAILS=" & '"'       &
                                             "-o" & '"';
   --        /SUFFIX_DETAILS=suffix
   --
   --   Use the given suffix as the suffix for the name of the file to place
   --   the detailed metrics into.

   S_Metric_Suppress : aliased constant S :=  "/SUPPRESS="                 &
                                               "NOTHING "                  &
                                                "!-nocc,!-noec,!-nonl,"    &
                                                "!-ne,!-nolocal "          &
                                               "CYCLOMATIC_COMPLEXITY "    &
                                                "-nocc "                   &
                                               "ESSENTIAL_COMPLEXITY "     &
                                                "-noec "                   &
                                               "MAXIMAL_LOOP_NESTING "     &
                                                "-nonl "                   &
                                               "EXITS_AS_GOTOS "           &
                                                "-ne "                     &
                                               "LOCAL_DETAILS "            &
                                                "-nolocal ";
   --  NODOC  (see /COMPLEXITY_METRICS /NO_LOCAL_DETAILS /NO_EXITS_AS_GOTOS)

   S_Metric_Verbose  : aliased constant S := "/VERBOSE "                   &
                                             "-v";
   --        /NOVERBOSE (D)
   --        /VERBOSE
   --
   --   Verbose mode.

   S_Metric_XMLout  : aliased constant S := "/XML_OUTPUT=@"                &
                                             "-ox@";
   --        /XML_OUTPUT=filename
   --
   --   Place the XML output into the specified file

   Metric_Switches : aliased constant Switches :=
                       (S_Metric_Add              'Access,
                        S_Metric_All_Prjs         'Access,
                        S_Metric_Complexity       'Access,
                        S_Metric_Coupling         'Access,
                        S_Metric_Debug            'Access,
                        S_Metric_Direct           'Access,
                        S_Metric_Element          'Access,
                        S_Metric_Ext              'Access,
                        S_Metric_Files            'Access,
                        S_Metric_Follow           'Access,
                        S_Metric_Format           'Access,
                        S_Metric_Globout          'Access,
                        S_Metric_Line             'Access,
                        S_Metric_Lines            'Access,
                        S_Metric_Mess             'Access,
                        S_Metric_No_Exits_As_Gotos'Access,
                        S_Metric_No_Local         'Access,
                        S_Metric_No_Static_Loop   'Access,
                        S_Metric_Project          'Access,
                        S_Metric_Quiet            'Access,
                        S_Metric_Suffix           'Access,
                        S_Metric_Subdirs          'Access,
                        S_Metric_Syntax           'Access,
                        S_Metric_Suppress         'Access,
                        S_Metric_Verbose          'Access,
                        S_Metric_XMLout           'Access);

   ----------------------------
   -- Switches for GNAT NAME --
   ----------------------------

   S_Name_Conf    : aliased constant S := "/CONFIG_FILE=<"                 &
                                            "-c>";
   --        /CONFIG_FILE=path_name
   --
   --   Create a configuration pragmas file 'path_name' (instead of the default
   --   'gnat.adc'). 'path_name' may include directory information. 'path_name'
   --   must be writable. There may be only one qualifier /CONFIG_FILE.
   --   This qualifier is not compatible with qualifier /PROJECT_FILE.

   S_Name_Dirs    : aliased constant S := "/SOURCE_DIRS=*"                 &
                                            "-d*";
   --        /SOURCE_DIRS=(directory, ...)
   --
   --   Look for source files in the specified directories. When this qualifier
   --   is specified, the current working directory will not be searched for
   --   source files, unless it is explicitly specified with a qualifier
   --   /SOURCE_DIRS or /DIRS_FILE. Several qualifiers /SOURCE_DIRS may be
   --   specified. If a directory is specified as a relative path, it is
   --   relative to the directory of the configuration pragmas file specified
   --   with qualifier /CONFIG_FILE, or to the directory of the project file
   --   specified with qualifier /PROJECT_FILE or, if neither qualifier
   --   /CONFIG_FILE nor qualifier /PROJECT_FILE are specified, it is relative
   --   to the current working directory. The directories specified with
   --   qualifiers /SOURCE_DIRS must exist and be readable.

   S_Name_Dfile   : aliased constant S := "/DIRS_FILE=<"                   &
                                            "-D>";
   --        /DIRS_FILE=file_name
   --
   --   Look for source files in all directories listed in text file
   --   'file_name'. 'file_name' must be an existing, readable text file.
   --   Each non empty line in the specified file must be a directory.
   --   Specifying qualifier /DIRS_FILE is equivalent to specifying as many
   --   qualifiers /SOURCE_DIRS as there are non empty lines in the specified
   --   text file.

   S_Name_Follow  : aliased constant S := "/FOLLOW_LINKS_FOR_FILES "       &
                                            "-eL";
   --        /NOFOLLOW_LINKS_FOR_FILES (D)
   --        /FOLLOW_LINKS_FOR_FILES
   --
   --    Follow links when parsing project files

   S_Name_Frng    : aliased constant S := "/FOREIGN_PATTERN=" & '"'        &
                                            "-f" & '"';
   --        /FOREIGN_PATTERN=<string>
   --
   --   Specify a foreign pattern.
   --   Using this qualifier, it is possible to add sources of languages other
   --   than Ada to the list of sources of a project file. It is only useful
   --   if a qualifier /PROJECT_FILE is used. For example,
   --
   --   GNAT NAME /PROJECT_FILE=PRJ /FOREIGN_PATTERN="*.C" "*.ADA"
   --
   --   will look for Ada units in all files with the '.ADA' extension, and
   --   will add to the list of file for project PRJ.GPR the C files with
   --   extension ".C".

   S_Name_Help    : aliased constant S := "/HELP "                         &
                                            "-h";
   --        /NOHELP (D)
   --        /HELP
   --
   --   Output usage information to the standard output stream.

   S_Name_Proj    : aliased constant S := "/PROJECT_FILE=<"                &
                                            "-P>";
   --        /PROJECT_FILE=file_name
   --
   --   Create or update a project file. 'file_name' may include directory
   --   information. The specified file must be writable. There may be only
   --   one qualifier /PROJECT_FILE. When a qualifier /PROJECT_FILE is
   --   specified, no qualifier /CONFIG_FILE may be specified.

   S_Name_Subdirs : aliased constant S := "/SUBDIRS=<"                     &
                                             "--subdirs=>";
   --        /SUBDIRS=dir
   --
   --   The actual directories (object, exec, library, ...) are subdirectories
   --   of the directory specified in the project file. If the subdirectory
   --   does not exist, it is created automatically.

   S_Name_Verbose : aliased constant S := "/VERBOSE "                      &
                                            "-v";
   --        /NOVERBOSE (D)
   --        /VERBOSE
   --
   --   Verbose mode. Output detailed explanation of behavior to the standard
   --   output stream. This includes name of the file written, the name of the
   --   directories to search and, for each file in those directories whose
   --   name matches at least one of the Naming Patterns, an indication of
   --   whether the file contains a unit, and if so the name of the unit.

   S_Name_Excl    : aliased constant S := "/EXCLUDED_PATTERN=" & '"'       &
                                            "-x" & '"';
   --      /EXCLUDED_PATTERN=<string>
   --
   --   Specify an excluded pattern.
   --   Using this qualifier, it is possible to exclude some files that would
   --   match the Naming patterns. For example,
   --
   --   GNAT NAME /EXCLUDED_PATTERN="*_NT.ADA" "*.ADA"
   --
   --   will look for Ada units in all files with the '.ADA' extension, except
   --   those whose names end with '_NT.ADA'.

   Name_Switches : aliased constant Switches :=
                     (S_Name_Conf    'Access,
                      S_Name_Dirs    'Access,
                      S_Name_Dfile   'Access,
                      S_Name_Follow  'Access,
                      S_Name_Frng    'Access,
                      S_Name_Help    'Access,
                      S_Name_Proj    'Access,
                      S_Name_Subdirs 'Access,
                      S_Name_Verbose 'Access,
                      S_Name_Excl    'Access);

   ----------------------------------
   -- Switches for GNAT PREPROCESS --
   ----------------------------------

   S_Prep_Assoc   : aliased constant S := "/ASSOCIATE=" & '"'              &
                                            "-D" & '"';
   --        /ASSOCIATE="name=val"
   --
   --   Defines a new symbol, associated with value. If no value is given
   --   on the command line, then symbol is considered to be True.
   --   This qualifier can be used in place of a definition file.

   S_Prep_Blank   : aliased constant S := "/BLANK_LINES "                  &
                                            "-b";
   --        /NOBLANK_LINES (D)
   --        /BLANK_LINES
   --
   --   Causes both preprocessor lines and the lines deleted by preprocessing
   --   to be replaced by blank lines in the output source file, thus
   --   preserving line numbers in the output file.

   S_Prep_Com     : aliased constant S := "/COMMENTS "                     &
                                            "-c";
   --        /NOCOMMENTS (D)
   --        /COMMENTS
   --
   --   /COMMENTS causes both preprocessor lines and the lines deleted
   --   by preprocessing to be retained in the output source as comments marked
   --   with the special string "--! ". This option will result in line numbers
   --   being preserved in the output file.
   --
   --   /NOCOMMENTS causes both preprocessor lines and the lines deleted by
   --   preprocessing to be replaced by blank lines in the output source file,
   --   thus preserving line numbers in the output file.

   S_Prep_Ref     : aliased constant S := "/REFERENCE "                    &
                                            "-r";
   --        /NOREFERENCE (D)
   --        /REFERENCE
   --
   --   Causes a "Source_Reference" pragma to be generated that references the
   --   original input file, so that error messages will use the file name of
   --   this original file.  Also implies /BLANK_LINES if /COMMENTS is not
   --   specified.

   S_Prep_Remove  : aliased constant S := "/REMOVE "                       &
                                            "!-b,!-c";
   --        /REMOVE (D)
   --        /NOREMOVE
   --
   --   Preprocessor lines and deleted lines are completely removed from the
   --   output.

   S_Prep_Replace : aliased constant S := "/REPLACE_IN_COMMENTS "          &
                                            "-C";
   --        /NOREPLACE_IN_COMMENTS (D)
   --        /REPLACE_IN_COMMENTS
   --
   --   Causes preprocessor to scan comments and perform replacements on
   --   any $symbol occurrences within the comment text.

   S_Prep_Symbols : aliased constant S := "/SYMBOLS "                      &
                                            "-s";
   --        /NOSYMBOLS (D)
   --        /SYMBOLS
   --
   --   Causes a sorted list of symbol names and values to be listed on
   --   SYS$OUTPUT.

   S_Prep_Undef   : aliased constant S := "/UNDEFINED "                    &
                                            "-u";
   --        /NOUNDEFINED (D)
   --        /UNDEFINED

   Prep_Switches : aliased constant Switches :=
                     (S_Prep_Assoc   'Access,
                      S_Prep_Blank   'Access,
                      S_Prep_Com     'Access,
                      S_Prep_Ref     'Access,
                      S_Prep_Remove  'Access,
                      S_Prep_Replace 'Access,
                      S_Prep_Symbols 'Access,
                      S_Prep_Undef   'Access);

   ------------------------------
   -- Switches for GNAT PRETTY --
   ------------------------------

   S_Pretty_Add    : aliased constant S := "/ADD_PROJECT_SEARCH_DIR=*"     &
                                             "-aP*";
   --        /ADD_PROJECT_SEARCH_PATH=(directory[,...])
   --
   --   Add directories to the project search path.

   S_Pretty_Align  : aliased constant S := "/ALIGN="                       &
                                           "DEFAULT "                      &
                                               "-A12345 "                  &
                                           "OFF "                          &
                                               "-A0 "                      &
                                           "COLONS "                       &
                                               "-A1 "                      &
                                           "DECLARATIONS "                 &
                                               "-A2 "                      &
                                           "STATEMENTS "                   &
                                               "-A3 "                      &
                                           "ARROWS "                       &
                                               "-A4 "                      &
                                           "COMPONENT_CLAUSES "            &
                                               "-A5";
   --        /ALIGN[=align-option, align-option, ...]
   --
   --   Set alignments. By default, all alignments (colons in declarations,
   --   initialisations in declarations, assignments and arrow delimiters) are
   --   ON.
   --
   --   align-option may be one of the following:
   --
   --      OFF (D)           Set all alignments to OFF
   --      COLONS            Set alignments of colons in declarations to ON
   --      DECLARATIONS      Set alignments of initialisations in declarations
   --                        to ON
   --      STATEMENTS        Set alignments of assignments statements to ON
   --      ARROWS            Set alignments of arrow delimiters to ON.
   --      COMPONENT_CLAUSES Set alignments of AT keywords in component
   --                        clauses ON
   --
   --   Specifying one of the ON options without first specifying the OFF
   --   option has no effect, because by default all alignments are set to ON.

   S_Pretty_All_Prjs : aliased constant S := "/ALL_PROJECTS "              &
                                              "-U";
   --        /NOALL_PROJECTS (D)
   --        /ALL_PROJECTS
   --   When GNAT PRETTY is used with a Project File and no source is
   --   specified, the underlying tool gnatpp is called for all the
   --   sources of all the Project Files in the project tree.

   S_Pretty_Attrib : aliased constant S := "/ATTRIBUTE_CASING="            &
                                           "MIXED_CASE "                   &
                                               "-aM "                      &
                                           "LOWER_CASE "                   &
                                               "-aL "                      &
                                           "UPPER_CASE "                   &
                                               "-aU";
   --        /ATTRIBUTE_CASING[=casing-option]
   --
   --   Set the case of the attributes. By default the attributes are in mixed
   --   case.
   --   casing-option may be one of the following:
   --
   --      MIXED_CASE (D)
   --      LOWER_CASE
   --      UPPER_CASE

   S_Pretty_Comments  : aliased constant S := "/COMMENTS_LAYOUT="          &
                                              "UNTOUCHED "                 &
                                                 "-c0 "                    &
                                              "DEFAULT "                   &
                                                 "-c1 "                    &
                                              "STANDARD_INDENT "           &
                                                 "-c2 "                    &
                                              "GNAT_BEGINNING "            &
                                                 "-c3 "                    &
                                              "REFORMAT "                  &
                                                 "-c4 "                    &
                                              "KEEP_SPECIAL "              &
                                                 "-c5";
   --        /COMMENTS_LAYOUT[=layout-option, layout-option, ...]
   --
   --   Set the comment layout. By default, comments use the GNAT style
   --   comment line indentation.
   --
   --   layout-option may be one of the following:
   --
   --     UNTOUCHED           All the comments remain unchanged
   --     DEFAULT (D)         GNAT style comment line indentation
   --     STANDARD_INDENT     Standard comment line indentation
   --     GNAT_BEGINNING      GNAT style comment beginning
   --     REFORMAT            Reformat comment blocks
   --     KEEP_SPECIAL        Keep unchanged special form comments
   --
   --     All combinations of layout options are allowed, except for DEFAULT
   --     and STANDARD_INDENT which are mutually exclusive, and also if
   --     UNTOUCHED is specified, this must be the only option.
   --
   --     The difference between "GNAT style comment line indentation" and
   --     "standard comment line indentation" is the following: for standard
   --     comment indentation, any comment line is indented as if it were
   --     a declaration or statement at the same place.
   --     For GNAT style comment indentation, comment lines which are
   --     immediately followed by if or case statement alternative, record
   --     variant or 'begin' keyword are indented as the keyword that follows
   --     them.:
   --
   --     Standard indentation:
   --
   --        if A then
   --           null;
   --           -- some comment
   --        else
   --           null;
   --        end if;
   --
   --     GNAT style indentation:
   --
   --        if A then
   --           null;
   --        -- some comment
   --        else
   --           null;
   --        end if;
   --
   --     Option "GNAT style comment beginning" means that for each comment
   --     which is not considered as non-formattable separator (that is, the
   --     comment line contains only dashes, or a comment line ends with two
   --     dashes), there will be at least two spaces between starting "--" and
   --     the first non-blank character of the comment.

   S_Pretty_Config    : aliased constant S := "/CONFIGURATION_PRAGMAS_FILE=<" &
                                              "-gnatec>";
   --        /CONFIGURATION_PRAGMAS_FILE=file
   --
   --   Specify a configuration pragmas file that need to be passed to the
   --   compiler.

   S_Pretty_Constr    : aliased constant S := "/CONSTRUCT_LAYOUT="         &
                                               "GNAT "                     &
                                                  "-l1 "                   &
                                               "COMPACT "                  &
                                                  "-l2 "                   &
                                               "UNCOMPACT "                &
                                                   "-l3";
   --        /CONSTRUCT_LAYOUT[=construct-option]
   --
   --   Set construct layout. Default is GNAT style layout.
   --   construct-option may be one of the following:
   --
   --      GNAT (D)
   --      COMPACT
   --      UNCOMPACT
   --
   --   The difference between GNAT style and Compact layout on one hand
   --   and Uncompact layout on the other hand can be illustrated by the
   --   following examples:
   --
   --       GNAT style and                          Uncompact layout
   --       Compact layout
   --
   --       type q is record                        type q is
   --          a : integer;                            record
   --          b : integer;                               a : integer;
   --       end record;                                   b : integer;
   --                                                  end record;
   --
   --
   --       Block : declare                         Block :
   --          A : Integer := 3;                       declare
   --       begin                                         A : Integer := 3;
   --          Proc (A, A);                            begin
   --       end Block;                                    Proc (A, A);
   --                                                  end Block;
   --
   --       Clear : for J in 1 .. 10 loop           Clear :
   --          A (J) := 0;                             for J in 1 .. 10 loop
   --       end loop Clear;                               A (J) := 0;
   --                                                  end loop Clear;
   --
   --
   --   A further difference between GNAT style layout and compact layout is
   --   that in GNAT style layout compound statements, return statements and
   --   bodies are always separated by empty lines.

   S_Pretty_Comind    : aliased constant S := "/CONTINUATION_INDENT=#"     &
                                                 "-cl#";
   --        /CONTINUATION_INDENT=nnn
   --
   --   Indentation level for continuation lines, nnn from 1 .. 9.
   --   The default value is one less then the (normal) indentation level,
   --   unless the indentation is set to 1: in that case the default value for
   --   continuation line indentation is also 1.

   S_Pretty_Compact_Is : aliased constant S := "/NO_SEPARATE_IS "          &
                                                 "--no-separate-is";
   --        /NO_SEPARATE_IS
   --
   --   Do not place the IS keyword on a separate line in a subprogram body in
   --   case if the specification occupies more then one line.

   S_Pretty_Sep_Label : aliased constant S := "/SEPARATE_LABEL "           &
                                                    "--separate-label";
   --        /SEPARATE_LABEL
   --
   --   Place statement label(s) and the statement itself on separate lines.

   S_Pretty_Sep_Loop_Then : aliased constant S := "/SEPARATE_LOOP_THEN "   &
                                                    "--separate-loop-then";
   --        /SEPARATE_LOOP_THEN
   --
   --   Place the THEN keyword in IF statement and the LOOP keyword in for-
   --   and while-loops on a separate line.

   S_Pretty_N_Sep_Loop_Then : aliased constant S := "/NO_SEPARATE_LOOP_THEN " &
                                                    "--no-separate-loop-then";
   --        /NO_SEPARATE_LOOP_THEN
   --
   --   Do not place the THEN keyword in IF statement and the LOOP keyword in
   --   for- and while-loops on a separate line.

   S_Pretty_Use_On_New_Line : aliased constant S := "/USE_ON_NEW_LINE "    &
                                                      "--use-on-new-line";
   --        /USE_ON_NEW_LINE
   --
   --   Start any USE clause that is a part of a context clause from a
   --   separate line.

   S_Pretty_Stnm_On_Nw_Line : aliased constant S := "/STMT_NAME_ON_NEW_LINE " &
                                                      "--separate-stmt-name";
   --        /STMT_NAME_ON_NEW_LINE
   --
   --   For named block and loop statements use a separate line for the
   --   statement name, but do not use an extra indentation level for the
   --   statement itself.

   S_Pretty_Eol       : aliased constant S := "/END_OF_LINE="              &
                                                "DOS "                     &
                                                   "--eol=dos "            &
                                                "UNIX "                    &
                                                   "--eol=unix "           &
                                                "CRLF "                    &
                                                   "--eol=crlf "           &
                                                "LF "                      &
                                                   "--eol=lf";
   --        /END_OF_LINE=[option]
   --
   --   Specifies the form of the line terminators in the produced source.
   --   By default, the form of the line terminator depends on the platforms.
   --   On Unix and VMS, it is a Line Feed (LF) character. On Windows (DOS),
   --   It is a Carriage Return (CR) followed by a Line Feed.
   --   The Options DOS and CRLF are equivalent. The options UNIX and LF are
   --   also equivalent.

   S_Pretty_Ext       : aliased constant S := "/EXTERNAL_REFERENCE=" & '"' &
                                            "-X" & '"';
   --        /EXTERNAL_REFERENCE="name=val"
   --
   --   Specifies an external reference to the project manager. Useful only if
   --   /PROJECT_FILE is used.
   --
   --   Example:
   --      /EXTERNAL_REFERENCE="DEBUG=TRUE"

   S_Pretty_Current   : aliased constant S := "/CURRENT_DIRECTORY "        &
                                              "!-I-";
   --        /CURRENT_DIRECTORY (D)
   --
   --   Look for source files in the current working directory.
   --
   --        /NOCURRENT_DIRECTORY
   --   Do not look for source files in the current working directory.

   S_Pretty_Dico      : aliased constant S := "/DICTIONARY=*"              &
                                              "-D*";
   --        /DICTIONARY=(file_name, ...)
   --
   --   Use each specified file as a dictionary file that defines the casing
   --   for a set of specified names, thereby overriding the effect on these
   --   names by any explicit or implicit /NAME_CASING qualifier.
   --
   --   GNAT PRETTY implicitly uses a default dictionary file to define the
   --   casing for the Ada predefined names and the names declared in the GNAT
   --   libraries.
   --
   --   The structure of a dictionary file, and details on the conventions
   --   used in the default dictionary file, are defined in the GNAT User's
   --   Guide.

   S_Pretty_Encoding  : aliased constant S := "/RESULT_ENCODING="          &
                                              "BRACKETS "                  &
                                                 "-Wb "                    &
                                              "HEX "                       &
                                                 "-Wh "                    &
                                              "UPPER "                     &
                                                 "-Wu "                    &
                                              "SHIFT_JIS "                 &
                                                 "-Ws "                    &
                                              "EUC "                       &
                                                 "-We "                    &
                                              "UTF8 "                      &
                                                 "-W8";
   --        /RESULT_ENCODING[=encoding-type]
   --
   --   Specify the wide character encoding method used when writing the
   --   reformatted code in the result file. 'encoding-type' is one of the
   --   following:
   --
   --      BRACKETS (D)      Brackets encoding.
   --
   --      HEX               Hex ESC encoding.
   --
   --      UPPER             Upper half encoding.
   --
   --      SHIFT_JIS         Shift-JIS encoding.
   --
   --      EUC               EUC Encoding.
   --
   --      UTF8              UTF-8 encoding.
   --
   --   See 'HELP GNAT COMPILE /WIDE_CHARACTER_ENCODING' for an explanation
   --   about the different character encoding methods.

   S_Pretty_Enums     : aliased constant S := "/ENUM_CASING="              &
                                              "AS_DECLARED "               &
                                                 "-neD "                   &
                                              "LOWER_CASE "                &
                                                 "-neL "                   &
                                              "UPPER_CASE "                &
                                                 "-neU "                   &
                                              "MIXED_CASE "                &
                                                 "-neM";
   --        /ENUM_CASING=name-option
   --
   --   Specify the casing of enumeration literals. If not specified, the
   --   casing of enumeration literals is defined by the NAME_CASING option.
   --   'name-option' may be one of:
   --
   --      AS_DECLARED       Literals casing for defining occurrences are
   --                        as they appear in the source file.
   --
   --      LOWER_CASE        Literals are in lower case.
   --
   --      UPPER_CASE        Literals are in upper case.
   --
   --      MIXED_CASE        Literals are in mixed case.

   S_Pretty_Files     : aliased constant S := "/FILES=@"                   &
                                                 "-files=@";
   --      /FILES=filename
   --
   --   Take as arguments the files that are listed in the specified
   --   text file.

   S_Pretty_Follow : aliased constant S := "/FOLLOW_LINKS_FOR_FILES "      &
                                             "-eL";
   --        /NOFOLLOW_LINKS_FOR_FILES (D)
   --        /FOLLOW_LINKS_FOR_FILES
   --
   --    Follow links when parsing project files

   S_Pretty_Forced    : aliased constant S := "/FORCED_OUTPUT=@"           &
                                                 "-of@";
   --        /FORCED_OUTPUT=file
   --
   --   Write the output into the specified file, overriding any possibly
   --   existing file.

   S_Pretty_Formfeed  : aliased constant S := "/FORM_FEED_AFTER_PRAGMA_PAGE " &
                                              "-ff";
   --        /FORM_FEED_AFTER_PRAGMA_PAGE
   --
   --   When there is a pragma Page in the source, insert a Form Feed
   --   character immediately after the semicolon that follows the pragma
   --   Page.

   S_Pretty_Indent    : aliased constant S := "/INDENTATION_LEVEL=#"       &
                                                "-i#";
   --        /INDENTATION_LEVEL=nnn
   --
   --   Specify the number of spaces to add for each indentation level.
   --   nnn must be between 1 and 9. The default is 3.

   S_Pretty_Keyword   : aliased constant S := "/KEYWORD_CASING="           &
                                              "LOWER_CASE "                &
                                                 "-kL "                    &
                                              "UPPER_CASE "                &
                                                 "-kU";
   --        /KEYWORD_CASING[=keyword-option]
   --
   --   Specify the case of Ada keywords. The default is keywords in lower
   --   case.
   --
   --   keyword-option may be one of the following:
   --
   --      LOWER_CASE (D)
   --      UPPER_CASE

   S_Pretty_Maxlen    : aliased constant S := "/LINE_LENGTH_MAX=#"         &
                                                 "-M#";
   --        /LINE_LENGTH_MAX=nnn
   --
   --   Set the maximum line length, nnn from 32 ..256. The default is 79.

   S_Pretty_Maxact    : aliased constant S := "/MAX_ACT=#"                 &
                                                 "--call_threshold=#";
   --        /MAX_ACT=nnn
   --
   --  If the number of parameter associations is greater than nnn and if at
   --  least one association uses named notation, start each association from
   --  a new line

   S_Pretty_Maxind    : aliased constant S := "/MAX_INDENT=#"              &
                                                 "-T#";
   --        /MAX_INDENT=nnn
   --
   --   Do not use an additional indentation level for case alternatives
   --   and variants if their number is nnn or more. The default is 10.
   --   If nnn is zero, an additional indentation level is used for any
   --   number of case alternatives and variants.

   S_Pretty_Maxpar    : aliased constant S := "/MAX_PAR=#"                 &
                                                 "--par_threshold=#";
   --        /MAX_PAR=nnn
   --
   --  If the number of parameter specifications is greater than nnn (or equal
   --  to nnn in case of a function), start each specification from a new line.
   --  The default value is 3.

   S_Pretty_Mess      : aliased constant S := "/MESSAGES_PROJECT_FILE="    &
                                            "DEFAULT "                     &
                                               "-vP0 "                     &
                                            "MEDIUM "                      &
                                               "-vP1 "                     &
                                            "HIGH "                        &
                                               "-vP2";
   --        /MESSAGES_PROJECT_FILE[=messages-option]
   --
   --   Specifies the "verbosity" of the parsing of project files.
   --   messages-option may be one of the following:
   --
   --      DEFAULT (D)  No messages are output if there is no error or warning.
   --
   --      MEDIUM       A small number of messages are output.
   --
   --      HIGH         A great number of messages are output, most of them not
   --                   being useful for the user.

   S_Pretty_Names     : aliased constant S := "/NAME_CASING="              &
                                              "AS_DECLARED "               &
                                                 "-nD "                    &
                                              "LOWER_CASE "                &
                                                 "-nL "                    &
                                              "UPPER_CASE "                &
                                                 "-nU "                    &
                                              "MIXED_CASE "                &
                                                 "-nM";
   --        /NAME_CASING[=name-option]
   --
   --   Specify the casing of names.
   --   'name-option' may be one of:
   --
   --      AS_DECLARED (D)   Name casing for defining occurrences are as they
   --                        appear in the source file.
   --
   --      LOWER_CASE        Names are in lower case.
   --
   --      UPPER_CASE        Names are in upper case.
   --
   --      MIXED_CASE        Names are in mixed case.

   S_Pretty_Replace_No_Backup : aliased constant S := "/REPLACE_NO_BACKUP " &
                                                 "-rnb";
   --        /REPLACE_NO_BACKUP
   --
   --   Replace the argument source with the pretty-printed source without
   --   creating any backup copy of the argument source.

   S_Pretty_No_Labels : aliased constant S := "/NO_MISSED_LABELS "         &
                                                 "-e";
   --        /NO_MISSED_LABELS
   --
   --   Do not insert missing end/exit labels. The end label is the name of
   --   a construct that may optionally appear at the end of the construct.
   --   This includes the names of packages and subprograms.
   --   Similarly, the exit label is the name of a loop that may appear as the
   --   argument of an exit statement within the loop. By default, GNAT PRETTY
   --   inserts these end/exit labels when they are absent in the original
   --   source. This qualifier /NO_MISSED_LABELS suppresses this insertion,
   --   so that the formatted source reflects the original.

   S_Pretty_Notabs    : aliased constant S := "/NOTABS "                   &
                                                 "-notabs";
   --        /NOTABS
   --
   --   Replace all tabulations in comments with spaces.

   S_Pretty_Output    : aliased constant S := "/OUTPUT=@"                  &
                                              "-o@";
   --        /OUTPUT=file
   --
   --   Write the output to the specified file. If the file already exists,
   --   an error is reported.

   S_Pretty_Override  : aliased constant S := "/OVERRIDING_REPLACE "       &
                                                 "-rf";
   --        /NOOVERRIDING_REPLACE (D)
   --        /OVERRIDING_REPLACE
   --
   --   Replace the argument source with the pretty-printed source and copy the
   --   argument source into filename.NPP, overriding any existing file if
   --   needed.

   S_Pretty_Pragma    : aliased constant S := "/PRAGMA_CASING="            &
                                              "MIXED_CASE "                &
                                                 "-pM "                    &
                                              "LOWER_CASE "                &
                                                 "-pL "                    &
                                              "UPPER_CASE "                &
                                                 "-pU";
   --        /PRAGMA_CASING[=pragma-option]
   --
   --   Set the case of pragma identifiers. The default is Mixed case.
   --   pragma-option may be one of the following:
   --
   --      MIXED_CASE (D)
   --      LOWER_CASE
   --      UPPER_CASE

   S_Pretty_Project   : aliased constant S := "/PROJECT_FILE=<"            &
                                                "-P>";
   --        /PROJECT_FILE=filename
   --
   --   Specifies the main project file to be used. The project files rooted
   --   at the main project file will be parsed before any other processing to
   --   set the building environment.

   S_Pretty_Replace   : aliased constant S := "/REPLACE "                  &
                                                 "-r";
   --        /NOREPLACE (D)
   --        /REPLACE
   --
   --   Replace the argument source with the pretty-printed source and copy the
   --   argument source into filename.NPP. If filename.NPP already exists,
   --   report an error and exit.

   S_Pretty_RTS       : aliased constant S := "/RUNTIME_SYSTEM=|"          &
                                               "--RTS=|";
   --        /RUNTIME_SYSTEM=xxx
   --
   --    Compile against an alternate runtime system named xxx or RTS-xxx.

   S_Pretty_Search    : aliased constant S := "/SEARCH=*"                  &
                                              "-I*";
   --        /SEARCH=(directory[,...])
   --
   --    When looking for source files also look in directories specified.

   S_Pretty_Specific  : aliased constant S := "/SPECIFIC_CASING "          &
                                              "-D-";
   --        /SPECIFIC_CASING
   --
   --   Do not use the default dictionary file; instead, use the casing
   --   defined by a qualifier /NAME_CASING and/or any explicit dictionary
   --   file specified by a qualifier /DICTIONARY.

   S_Pretty_Standard  : aliased constant S := "/STANDARD_OUTPUT "          &
                                              "-pipe";
   --        /NOSTANDARD_OUTPUT (D)
   --        /STANDARD_OUTPUT
   --
   --   Redirect the output to the standard output.

   S_Pretty_Subdirs : aliased constant S := "/SUBDIRS=<"                   &
                                               "--subdirs=>";
   --        /SUBDIRS=dir
   --
   --   The actual directories (object, exec, library, ...) are subdirectories
   --   of the directory specified in the project file. If the subdirectory
   --   does not exist, it is created automatically.

   S_Pretty_Types     : aliased constant S := "/TYPE_CASING="              &
                                              "AS_DECLARED "               &
                                                 "-ntD "                   &
                                              "LOWER_CASE "                &
                                                 "-ntL "                   &
                                              "UPPER_CASE "                &
                                                 "-ntU "                   &
                                              "MIXED_CASE "                &
                                                 "-ntM";
   --        /TYPE_CASING=name-option
   --
   --   Specify the casing of type and subtype. If not specified, the
   --   casing of these names is defined by the NAME_CASING option.
   --   'name-option' may be one of:
   --
   --      AS_DECLARED       Name casing for defining occurrences are
   --                        as they appear in the source file.
   --
   --      LOWER_CASE        Namess are in lower case.
   --
   --      UPPER_CASE        Namess are in upper case.
   --
   --      MIXED_CASE        Namess are in mixed case.

   S_Pretty_Verbose   : aliased constant S := "/VERBOSE "                  &
                                              "-v";
   --        /NOVERBOSE (D)
   --        /VERBOSE
   --
   --   Verbose mode; GNAT PRETTY generates version information and then a
   --   trace of the actions it takes to produce or obtain the ASIS tree.

   S_Pretty_Warnings  : aliased constant S := "/WARNINGS "                 &
                                              "-w";
   --        /NOWARNINGS (D)
   --        /WARNINGS
   --
   --   Issue a warning to the standard error stream if it is not possible
   --   to provide the required layout in the result source.
   --   By default such warnings are not activated.

   Pretty_Switches : aliased constant Switches :=
                       (S_Pretty_Add              'Access,
                        S_Pretty_Align            'Access,
                        S_Pretty_All_Prjs         'Access,
                        S_Pretty_Attrib           'Access,
                        S_Pretty_Comments         'Access,
                        S_Pretty_Compact_Is       'Access,
                        S_Pretty_Config           'Access,
                        S_Pretty_Constr           'Access,
                        S_Pretty_Comind           'Access,
                        S_Pretty_Current          'Access,
                        S_Pretty_Dico             'Access,
                        S_Pretty_Eol              'Access,
                        S_Pretty_Ext              'Access,
                        S_Pretty_Encoding         'Access,
                        S_Pretty_Enums            'Access,
                        S_Pretty_Files            'Access,
                        S_Pretty_Follow           'Access,
                        S_Pretty_Forced           'Access,
                        S_Pretty_Formfeed         'Access,
                        S_Pretty_Indent           'Access,
                        S_Pretty_Keyword          'Access,
                        S_Pretty_Maxlen           'Access,
                        S_Pretty_Maxact           'Access,
                        S_Pretty_Maxind           'Access,
                        S_Pretty_Maxpar           'Access,
                        S_Pretty_Mess             'Access,
                        S_Pretty_Names            'Access,
                        S_Pretty_No_Labels        'Access,
                        S_Pretty_Notabs           'Access,
                        S_Pretty_Output           'Access,
                        S_Pretty_Override         'Access,
                        S_Pretty_Pragma           'Access,
                        S_Pretty_Replace          'Access,
                        S_Pretty_Replace_No_Backup'Access,
                        S_Pretty_Project          'Access,
                        S_Pretty_RTS              'Access,
                        S_Pretty_Search           'Access,
                        S_Pretty_Sep_Label        'Access,
                        S_Pretty_Sep_Loop_Then    'Access,
                        S_Pretty_N_Sep_Loop_Then  'Access,
                        S_Pretty_Subdirs          'Access,
                        S_Pretty_Use_On_New_Line  'Access,
                        S_Pretty_Stnm_On_Nw_Line  'Access,
                        S_Pretty_Specific         'Access,
                        S_Pretty_Standard         'Access,
                        S_Pretty_Types            'Access,
                        S_Pretty_Verbose          'Access,
                        S_Pretty_Warnings         'Access);

   ------------------------------
   -- Switches for GNAT SHARED --
   ------------------------------

   S_Shared_Debug   : aliased constant S := "/DEBUG="                      &
                                            "ALL "                         &
                                               "-g3 "                      &
                                            "NONE "                        &
                                               "-g0 "                      &
                                            "TRACEBACK "                   &
                                               "-g1 "                      &
                                            "NOTRACEBACK "                 &
                                               "-g0";
   --        /DEBUG[=debug-option]
   --        /NODEBUG
   --
   --   Specifies the amount of debugging information included. 'debug-option'
   --   is one of the following:
   --
   --        ALL (D)      Include full debugging information.
   --
   --        NONE         Provide no debugging information. Same as /NODEBUG.
   --
   --        TRACEBACK    Provide sufficient debug information for a traceback.
   --
   --        NOTRACEBACK  Same as NONE.

   S_Shared_Image  : aliased constant S := "/IMAGE=@"                      &
                                            "-o@";
   --        /IMAGE=image-name
   --
   --   'image-name' specifies the name for the generated shared library.

   S_Shared_Ident   : aliased constant S := "/IDENTIFICATION=" & '"'       &
                                            "--for-linker=IDENT="          &
                                            '"';
   --        /IDENTIFICATION="<string>"
   --
   --   "<string>" specifies the string to be stored in the image file ident-
   --   ification field in the image header. It overrides any pragma Ident
   --   specified string.

   S_Shared_Nofiles : aliased constant S := "/NOSTART_FILES "              &
                                            "-nostartfiles";
   --        /NOSTART_FILES
   --
   --   Link in default image initialization and startup functions.

   S_Shared_Noinhib : aliased constant S := "/NOINHIBIT-IMAGE "            &
                                            "--for-linker=--noinhibit-exec";
   --        /NOINHIBIT-IMAGE
   --
   --   Delete image if there are errors or warnings.

   S_Shared_Verb    : aliased constant S := "/VERBOSE "                    &
                                            "-v";
   --        /NOVERBOSE (D)
   --        /VERBOSE
   --
   --   Causes additional information to be output, including a full list of
   --   the included object files. This switch option is most useful when you
   --   want to see what set of object files are being used in the link step.

   S_Shared_ZZZZZ   : aliased constant S := "/<other> "                    &
                                            "--for-linker=";
   --        /<other>
   --
   --   Any other switch transmitted to the underlying linker.

   Shared_Switches : aliased constant Switches :=
                       (S_Shared_Debug   'Access,
                        S_Shared_Image   'Access,
                        S_Shared_Ident   'Access,
                        S_Shared_Nofiles 'Access,
                        S_Shared_Noinhib 'Access,
                        S_Shared_Verb    'Access,
                        S_Shared_ZZZZZ   'Access);

   -----------------------------
   -- Switches for GNAT STACK --
   -----------------------------

   S_Stack_Add        : aliased constant S := "/ADD_PROJECT_SEARCH_DIR=*"  &
                                                "-aP*";
   --        /ADD_PROJECT_SEARCH_PATH=(directory[,...])
   --
   --   Add directories to the project search path.

   S_Stack_All        : aliased constant S := "/ALL_SUBPROGRAMS "          &
                                                "-a";
   --        /NOALL_SUBPROGRAMS (D)
   --        /ALL_SUBPROGRAMS
   --
   --   Consider all subprograms as entry points.

   S_Stack_All_Cycles : aliased constant S := "/ALL_CYCLES "               &
                                                "-ca";
   --        /NOALL_CYCLES (D)
   --        /ALL_CYCLES
   --
   --   Extract all possible cycles in the call graph.

   S_Stack_All_Prjs   : aliased constant S := "/ALL_PROJECTS "             &
                                                "-U";
   --        /NOALL_PROJECTS (D)
   --        /ALL_PROJECTS
   --
   --   When GNAT STACK is used with a Project File and no source is
   --   specified, the underlying tool gnatstack is called for all the
   --   units of all the Project Files in the project tree.

   S_Stack_Debug      : aliased constant S := "/DEBUG "                    &
                                                "-g";
   --        /NODEBUG (D)
   --        /DEBUG
   --
   --   Generate internal debug information.

   S_Stack_Directory  : aliased constant S := "/DIRECTORY=*"               &
                                                "-aO*";
   --        /DIRECTORY=(direc[,...])
   --
   --   When looking for .ci files look also in directories specified.

   S_Stack_Entries    : aliased constant S := "/ENTRIES=*"                 &
                                                "-e*";
   --
   --        /ENTRY=(entry_point[,...])
   --
   --   Name of symbol to be used as entry point for the analysis.

   S_Stack_Files      : aliased constant S := "/FILES=@"                   &
                                                "-files=@";
   --      /FILES=filename
   --
   --   Take as arguments the files that are listed in the specified
   --   text file.

   S_Stack_Follow : aliased constant S := "/FOLLOW_LINKS_FOR_FILES "       &
                                            "-eL";
   --        /NOFOLLOW_LINKS_FOR_FILES (D)
   --        /FOLLOW_LINKS_FOR_FILES
   --
   --    Follow links when parsing project files

   S_Stack_Help       : aliased constant S := "/HELP "                     &
                                                "-h";
   --        /NOHELP (D)
   --        /HELP
   --
   --   Output a message explaining the usage of gnatstack.

   S_Stack_List       : aliased constant S := "/LIST=#"                    &
                                                "-l#";
   --        /LIST=nnn
   --
   --   Print the nnn subprograms requiring the biggest local stack usage. By
   --   default none will be displayed.

   S_Stack_Order      : aliased constant S := "/ORDER="                    &
                                              "STACK "                     &
                                                 "-os "                    &
                                              "ALPHABETICAL "              &
                                                 "-oa";
   --        /ORDER[=order-option]
   --
   --   Specifies the order for displaying the different call graphs.
   --   order-option may be one of the following:
   --
   --      STACK (D)    Select stack usage order
   --
   --      ALPHABETICAL Select alphabetical order

   S_Stack_Path       : aliased constant S := "/PATH "                     &
                                                "-p";
   --        /NOPATH (D)
   --        /PATH
   --
   --   Print all the subprograms that make up the worst-case path for every
   --   entry point.

   S_Stack_Project    : aliased constant S := "/PROJECT_FILE=<"            &
                                                "-P>";
   --        /PROJECT_FILE=filename
   --
   --   Specifies the main project file to be used. The project files rooted
   --   at the main project file will be parsed before the invocation of
   --   gnatstack.

   S_Stack_Output     : aliased constant S := "/OUTPUT=@"                  &
                                                "-f@";
   --        /OUTPUT=filename
   --
   --   Name of the file containing the generated graph (VCG format).

   S_Stack_Regexp     : aliased constant S := "/EXPRESSION=|"              &
                                                "-r|";
   --
   --        /EXPRESSION=regular-expression
   --
   --   Any symbol matching the regular expression will be considered as a
   --   potential entry point for the analysis.

   S_Stack_Subdirs : aliased constant S := "/SUBDIRS=<"                    &
                                              "--subdirs=>";
   --        /SUBDIRS=dir
   --
   --   The actual directories (object, exec, library, ...) are subdirectories
   --   of the directory specified in the project file. If the subdirectory
   --   does not exist, it is created automatically.

   S_Stack_Unbounded  : aliased constant S := "/UNBOUNDED=#"               &
                                                "-d#";
   --        /UNBOUNDED=nnn
   --
   --   Default stack size to be used for unbounded (dynamic) frames.

   S_Stack_Unknown    : aliased constant S := "/UNKNOWN=#"                 &
                                                "-u#";
   --        /UNKNOWN=nnn
   --
   --   Default stack size to be used for unknown (external) calls.

   S_Stack_Verbose    : aliased constant S := "/VERBOSE "                  &
                                                "-v";
   --        /NOVERBOSE (D)
   --        /VERBOSE
   --
   --   Specifies the amount of information to be displayed about the
   --   different subprograms. In verbose mode the full location of the
   --   subprogram will be part of the output, as well as detailed information
   --   about inaccurate data.

   S_Stack_Warnings   : aliased constant S := "/WARNINGS="                 &
                                              "ALL "                       &
                                                 "-Wa "                    &
                                              "CYCLES "                    &
                                                 "-Wc "                    &
                                              "UNBOUNDED "                 &
                                                 "-Wu "                    &
                                              "EXTERNAL "                  &
                                                 "-We "                    &
                                              "INDIRECT "                  &
                                                 "-Wi";
   --        /WARNINGS[=(keyword[,...])]
   --
   --    The following keywords are supported:
   --
   --        ALL        Turn on all optional warnings
   --
   --        CYCLES     Turn on warnings for cycles
   --
   --        UNBOUNDED  Turn on warnings for unbounded frames
   --
   --        EXTERNAL   Turn on warnings for external calls
   --
   --        INDIRECT   Turn on warnings for indirect calls

   Stack_Switches : aliased constant Switches :=
                      (S_Stack_Add        'Access,
                       S_Stack_All        'Access,
                       S_Stack_All_Cycles 'Access,
                       S_Stack_All_Prjs   'Access,
                       S_Stack_Debug      'Access,
                       S_Stack_Directory  'Access,
                       S_Stack_Entries    'Access,
                       S_Stack_Files      'Access,
                       S_Stack_Follow     'Access,
                       S_Stack_Help       'Access,
                       S_Stack_List       'Access,
                       S_Stack_Order      'Access,
                       S_Stack_Path       'Access,
                       S_Stack_Project    'Access,
                       S_Stack_Output     'Access,
                       S_Stack_Regexp     'Access,
                       S_Stack_Subdirs    'Access,
                       S_Stack_Unbounded  'Access,
                       S_Stack_Unknown    'Access,
                       S_Stack_Verbose    'Access,
                       S_Stack_Warnings   'Access);

   ----------------------------
   -- Switches for GNAT STUB --
   ----------------------------

   S_Stub_Add     : aliased constant S := "/ADD_PROJECT_SEARCH_DIR=*"      &
                                            "-aP*";
   --        /ADD_PROJECT_SEARCH_PATH=(directory[,...])
   --
   --   Add directories to the project search path.

   S_Stub_Config  : aliased constant S := "/CONFIGURATION_PRAGMAS_FILE=<"  &
                                            "-gnatec>";
   --        /CONFIGURATION_PRAGMAS_FILE=filespec
   --
   --   Specifies a configuration pragmas file that must be taken into account
   --   when compiling.

   S_Stub_Current : aliased constant S := "/CURRENT_DIRECTORY "            &
                                            "!-I-";
   --        /CURRENT_DIRECTORY (D)
   --        /NOCURRENT_DIRECTORY
   --
   --   Look for source, library or object files in the default directory.

   S_Stub_Ext     : aliased constant S := "/EXTERNAL_REFERENCE=" & '"'     &
                                            "-X" & '"';
   --        /EXTERNAL_REFERENCE="name=val"
   --
   --   Specifies an external reference to the project manager. Useful only if
   --   /PROJECT_FILE is used.
   --
   --   Example:
   --      /EXTERNAL_REFERENCE="DEBUG=TRUE"

   S_Stub_Follow  : aliased constant S := "/FOLLOW_LINKS_FOR_FILES "       &
                                            "-eL";
   --        /NOFOLLOW_LINKS_FOR_FILES (D)
   --        /FOLLOW_LINKS_FOR_FILES
   --
   --    Follow links when parsing project files

   S_Stub_Full    : aliased constant S := "/FULL "                         &
                                            "-f";
   --        /NOFULL (D)
   --        /FULL
   --
   --   If the destination directory already contains a file with the name of
   --   the body file for the argument file spec, replace it with the generated
   --   body stub. If /FULL is not used and there is already a body file, this
   --   existing body file is not replaced.

   S_Stub_Header  : aliased constant S := "/HEADER="                       &
                                            "GENERAL "                     &
                                               "-hg "                      &
                                            "SPEC "                        &
                                               "-hs";
   --        /HEADER[=header-option]
   --
   --   Specifies the form of the comment header above the generated body stub.
   --   If no /HEADER qualifier is specified, there is no comment header.
   --   header-option is one of the following:
   --
   --
   --      GENERAL (D)  Put a sample comment header into the body stub.
   --
   --      SPEC         Put the comment header (i.e., all the comments
   --                   preceding the compilation unit) from the source of the
   --                   library unit declaration into the body stub.

   S_Stub_Header_File : aliased constant S := "/FROM_HEADER_FILE=<" &
                                                "--header-file=>";

   --        /FROM_HEADER_FILE==filename
   --
   --   Use the content of the file as the comment header for a generated body
   --   stub.

   S_Stub_Indent  : aliased constant S := "/INDENTATION=#"                 &
                                            "-i#";
   --        /INDENTATION=nnn
   --
   --   (nnn is a non-negative integer). Set the indentation level in the
   --   generated body stub to nnn. nnn=0 means "no indentation".
   --   Default indentation is 3.

   S_Stub_Keep    : aliased constant S := "/KEEP "                         &
                                            "-k";
   --        /NOKEEP (D)
   --        /KEEP
   --
   --   Do not delete the tree file (i.e., the snapshot of the compiler
   --   internal structures used by gnatstub) after creating the body stub.

   S_Stub_Length  : aliased constant S := "/LINE_LENGTH=#"                 &
                                            "-l#";
   --        /LINE_LENGTH=nnn
   --
   --   (n is a non-negative integer). Set the maximum line length in the body
   --   stub to nnn. Default is 78.

   S_Stub_Mess    : aliased constant S := "/MESSAGES_PROJECT_FILE="        &
                                            "DEFAULT "                     &
                                               "-vP0 "                     &
                                            "MEDIUM "                      &
                                               "-vP1 "                     &
                                            "HIGH "                        &
                                               "-vP2";
   --        /MESSAGES_PROJECT_FILE[=messages-option]
   --
   --   Specifies the "verbosity" of the parsing of project files.
   --   messages-option may be one of the following:
   --
   --      DEFAULT (D)  No messages are output if there is no error or warning.
   --
   --      MEDIUM       A small number of messages are output.
   --
   --      HIGH         A great number of messages are output, most of them not
   --                   being useful for the user.

   S_Stub_No_Exc  : aliased constant S := "/NO_EXCEPTION "                 &
                                          "--no-exception";
   --        /NONO_EXCEPTION (D)
   --        /NO_EXCEPTION
   --
   --  Avoid raising PROGRAM_ERROR in the generated program unit stubs.

   S_Stub_No_Head : aliased constant S := "/NO_LOCAL_HEADER "             &
                                          "--no-local-header";
   --        /NONO_LOCAL_HEADER (D)
   --        /NO_LOCAL_HEADER
   --
   --  Do not put local comment header before body stub for local program unit.

   S_Stub_Output  : aliased constant S := "/OUTPUT=@"                      &
                                            "-o@";
   --        /OUTPUT=filespec
   --
   --   Body file name. This should be set if the argument file name does not
   --   follow the GNAT file naming conventions. If this switch is omitted,
   --   the default name for the body will be obtained from the argument file
   --   name according to the GNAT file naming conventions.

   S_Stub_Project : aliased constant S := "/PROJECT_FILE=<"                &
                                            "-P>";
   --        /PROJECT_FILE=filename
   --
   --   Specifies the main project file to be used. The project files rooted
   --   at the main project file will be parsed before any other processing.
   --   The source and object directories to be searched will be communicated
   --   to gnatstub through logical names ADA_PRJ_INCLUDE_FILE and
   --   ADA_PRJ_OBJECTS_FILE.

   S_Stub_Quiet   : aliased constant S := "/QUIET "                        &
                                            "-q";
   --        /NOQUIET (D)
   --        /QUIET
   --
   --   Quiet mode: do not generate a confirmation when a body is successfully
   --   created, and do not generate a message when a body is not required for
   --   an argument unit.

   S_Stub_Search  : aliased constant S := "/SEARCH=*"                      &
                                            "-I*";
   --        /SEARCH=(directory[,...])
   --
   --    When looking for source files also look in directories specified.

   S_Stub_Subdirs : aliased constant S := "/SUBDIRS=<"                     &
                                             "--subdirs=>";
   --        /SUBDIRS=dir
   --
   --   The actual directories (object, exec, library, ...) are subdirectories
   --   of the directory specified in the project file. If the subdirectory
   --   does not exist, it is created automatically.

   S_Stub_Tree    : aliased constant S := "/TREE_FILE="                    &
                                            "OVERWRITE "                   &
                                               "-t "                       &
                                            "SAVE "                        &
                                               "-k "                       &
                                            "REUSE "                       &
                                               "-r";
   --        /TREE_FILE[=treefile-option]
   --
   --   Specify what to do with the tree file.
   --   treefile-option is one of the following:
   --
   --      OVERWRITE (D)  Overwrite the existing tree file. If the current
   --                     directory already contains the file which, according
   --                     to the GNAT file naming rules should be considered
   --                     as a tree file for the argument source file, gnatstub
   --                     will refuse to create the tree file needed to create
   --                     a sample body unless this option is chosen.
   --
   --      SAVE           Do not remove the tree file (i.e., the snapshot
   --                     of the compiler internal structures used by gnatstub)
   --                     after creating the body stub.
   --
   --      REUSE          Reuse the tree file (if it exists) instead of
   --                     creating it.
   --                     Instead of creating the tree file for the library
   --                     unit declaration, gnatstub tries to find it in the
   --                     current directory and use it for creating a body.
   --                     If the tree file is not found, no body is created.
   --                     This option also implies `SAVE', whether or not the
   --                     latter is set explicitly.

   S_Stub_Verbose : aliased constant S := "/VERBOSE "                      &
                                            "-v";
   --        /NOVERBOSE (D)
   --        /VERBOSE
   --
   --   Verbose mode: generate version information.

   Stub_Switches : aliased constant Switches :=
                     (S_Stub_Add        'Access,
                      S_Stub_Config     'Access,
                      S_Stub_Current    'Access,
                      S_Stub_Ext        'Access,
                      S_Stub_Follow     'Access,
                      S_Stub_Full       'Access,
                      S_Stub_Header     'Access,
                      S_Stub_Header_File'Access,
                      S_Stub_Indent     'Access,
                      S_Stub_Keep       'Access,
                      S_Stub_Length     'Access,
                      S_Stub_Mess       'Access,
                      S_Stub_Output     'Access,
                      S_Stub_Project    'Access,
                      S_Stub_No_Exc     'Access,
                      S_Stub_No_Head    'Access,
                      S_Stub_Quiet      'Access,
                      S_Stub_Search     'Access,
                      S_Stub_Subdirs    'Access,
                      S_Stub_Tree       'Access,
                      S_Stub_Verbose    'Access);

   ----------------------------
   -- Switches for GNAT SYNC --
   ----------------------------

   S_Sync_Add    : aliased constant S := "/ADD_PROJECT_SEARCH_DIR=*"       &
                                            "-aP*";
   --        /ADD_PROJECT_SEARCH_PATH=(directory[,...])
   --
   --   Add directories to the project search path.

   S_Sync_All    : aliased constant S := "/ALL "                           &
                                            "-a";
   --        /NOALL (D)
   --        /ALL
   --
   --   Also check the components of the GNAT run time and process the needed
   --  components of the GNAT RTL when building and analyzing the global
   --  structure for checking the global rules.

   S_Sync_Allproj : aliased constant S := "/ALL_PROJECTS "                 &
                                            "-U";
   --        /NOALL_PROJECTS (D)
   --        /ALL_PROJECTS
   --
   --   When GNAT SYNC is used with a Project File and no source is
   --   specified, the underlying tool gnatsync is called for all the
   --   sources of all the Project Files in the project tree.

   S_Sync_Ext     : aliased constant S := "/EXTERNAL_REFERENCE=" & '"'     &
                                             "-X" & '"';
   --       /EXTERNAL_REFERENCE="name=val"
   --
   --   Specifies an external reference to the project manager. Useful only if
   --   /PROJECT_FILE is used.
   --
   --   Example:
   --      /EXTERNAL_REFERENCE="DEBUG=TRUE"

   S_Sync_Files  : aliased constant S := "/FILES=@"                        &
                                             "-files=@";
   --      /FILES=filename
   --
   --   Take as arguments the files that are listed in the specified
   --   text file.

   S_Sync_Follow  : aliased constant S := "/FOLLOW_LINKS_FOR_FILES "       &
                                            "-eL";
   --        /NOFOLLOW_LINKS_FOR_FILES (D)
   --        /FOLLOW_LINKS_FOR_FILES
   --
   --    Follow links when parsing project files

   S_Sync_Main    : aliased constant S := "/MAIN_SUBPROGRAM=@"             &
                                            "-main=@";
   --        /MAIN_SUBPROGRAM=filename
   --
   --   Specify the name of the file containing the main subprogram

   S_Sync_Mess    : aliased constant S := "/MESSAGES_PROJECT_FILE="        &
                                             "DEFAULT "                    &
                                                "-vP0 "                    &
                                             "MEDIUM "                     &
                                                "-vP1 "                    &
                                             "HIGH "                       &
                                                "-vP2";
   --        /MESSAGES_PROJECT_FILE[=messages-option]
   --
   --   Specifies the "verbosity" of the parsing of project files.
   --   messages-option may be one of the following:
   --
   --      DEFAULT (D)  No messages are output if there is no error or warning.
   --
   --      MEDIUM       A small number of messages are output.
   --
   --      HIGH         A great number of messages are output, most of them not
   --                   being useful for the user.

   S_Sync_Project : aliased constant S := "/PROJECT_FILE=<"                &
                                             "-P>";
   --        /PROJECT_FILE=filename
   --
   --   Specifies the main project file to be used. The project files rooted
   --   at the main project file will be parsed before the invocation of the
   --   gnatcheck. The source directories to be searched will be communicated
   --   to gnatcheck through logical name ADA_PRJ_INCLUDE_FILE.

   S_Sync_Quiet  : aliased constant S := "/QUIET "                         &
                                            "-q";
   --        /NOQUIET (D)
   --        /QUIET
   --
   --   Work quietly, only output warnings and errors.

   S_Sync_Subdirs : aliased constant S := "/SUBDIRS=<"                     &
                                             "--subdirs=>";
   --        /SUBDIRS=dir
   --
   --   The actual directories (object, exec, library, ...) are subdirectories
   --   of the directory specified in the project file. If the subdirectory
   --   does not exist, it is created automatically.

   S_Sync_Verb   : aliased constant S := "/VERBOSE "                       &
                                            "-v";
   --        /NOVERBOSE (D)
   --        /VERBOSE
   --
   --   The version number and copyright notice are output, as well as exact
   --   copies of the gnat1 commands spawned to obtain the chop control
   --   information.

   S_Sync_Exec   : aliased constant S := "/EXECUTION_TIME "                &
                                            "-t";
   --        /NOEXECUTION_TIME (D)
   --        /EXECUTION_TIME
   --
   --   Output the execution time

   S_Sync_Details : aliased constant S := "/DETAILS="                      &
                                             "MEDIUM "                     &
                                               "-om "                      &
                                             "SHORT "                      &
                                               "-os "                      &
                                             "FULL "                       &
                                               "-of";
   --         /DETAILS[=options]
   --
   --   Specifies the details of the output.
   --   Options may be one of the following:
   --
   --       MEDIUM (D)
   --       SHORT
   --       FULL

   S_Sync_Warnoff : aliased constant S := "/WARNINGS_OFF "                 &
                                             "-wq";
   --
   --         /WARNINGS_OFF
   --
   --   Turn warnings off

   S_Sync_Output  : aliased constant S := "/OUTPUT_FILE=<"                 &
                                             "-out_file=>";
   --
   --        /OUTPUT_FILE=filename
   --
   --   Redirect output to a text file

   Sync_Switches : aliased constant Switches :=
                      (S_Sync_Add      'Access,
                       S_Sync_All      'Access,
                       S_Sync_Allproj  'Access,
                       S_Sync_Ext      'Access,
                       S_Sync_Follow   'Access,
                       S_Sync_Files    'Access,
                       S_Sync_Main     'Access,
                       S_Sync_Mess     'Access,
                       S_Sync_Project  'Access,
                       S_Sync_Quiet    'Access,
                       S_Sync_Subdirs  'Access,
                       S_Sync_Verb     'Access,
                       S_Sync_Exec     'Access,
                       S_Sync_Details  'Access,
                       S_Sync_Warnoff  'Access,
                       S_Sync_Output   'Access);

   ----------------------------
   -- Switches for GNAT XREF --
   ----------------------------

   S_Xref_Add     : aliased constant S := "/ADD_PROJECT_SEARCH_DIR=*"      &
                                            "-aP*";
   --        /ADD_PROJECT_SEARCH_PATH=(directory[,...])
   --
   --   Add directories to the project search path.

   S_Xref_All     : aliased constant S := "/ALL_FILES "                    &
                                            "-a";
   --        /NOALL_FILES (D)
   --        /ALL_FILES
   --
   --   If this switch is present, FIND and XREF will parse the read-only
   --   files found in the library search path. Otherwise, these files will
   --   be ignored. This option can be used to protect Gnat sources or your
   --   own libraries from being parsed, thus making FIND and XREF much
   --   faster, and their output much smaller.

   S_Xref_Deriv   : aliased constant S := "/DERIVED_TYPES "                &
                                            "-d";
   --        /NODERIVED_TYPES (D)
   --        /DERIVED_TYPES
   --
   --   Output the parent type reference for each matching derived types.

   S_Xref_Ext     : aliased constant S := "/EXTERNAL_REFERENCE=" & '"'     &
                                            "-X" & '"';
   --        /EXTERNAL_REFERENCE="name=val"
   --
   --   Specifies an external reference to the project manager. Useful only if
   --   /PROJECT_FILE is used.
   --
   --   Example:
   --      /EXTERNAL_REFERENCE="DEBUG=TRUE"

   S_Xref_Follow  : aliased constant S := "/FOLLOW_LINKS_FOR_FILES "       &
                                            "-eL";
   --        /NOFOLLOW_LINKS_FOR_FILES (D)
   --        /FOLLOW_LINKS_FOR_FILES
   --
   --    Follow links when parsing project files

   S_Xref_Full    : aliased constant S := "/FULL_PATHNAME "                &
                                            "-f";
   --        /NOFULL_PATHNAME (D)
   --        /FULL_PATHNAME
   --
   --   If this switch is set, the output file names will be preceded by their
   --   directory (if the file was found in the search path). If this switch
   --   is not set, the directory will not be printed.

   S_Xref_Global  : aliased constant S := "/IGNORE_LOCALS "                &
                                            "-g";
   --        /NOIGNORE_LOCALS (D)
   --        /IGNORE_LOCALS
   --
   --   If this switch is set, information is output only for library-level
   --   entities, ignoring local entities. The use of this switch may
   --   accelerate FIND and XREF.

   S_Xref_Mess    : aliased constant S := "/MESSAGES_PROJECT_FILE="        &
                                            "DEFAULT "                     &
                                               "-vP0 "                     &
                                            "MEDIUM "                      &
                                               "-vP1 "                     &
                                            "HIGH "                        &
                                               "-vP2";
   --        /MESSAGES_PROJECT_FILE[=messages-option]
   --
   --   Specifies the "verbosity" of the parsing of project files.
   --   messages-option may be one of the following:
   --
   --      DEFAULT (D)  No messages are output if there is no error or warning.
   --
   --      MEDIUM       A small number of messages are output.
   --
   --      HIGH         A great number of messages are output, most of them not
   --                   being useful for the user.

   S_Xref_Nostinc : aliased constant S := "/NOSTD_INCLUDES "               &
                                            "-nostdinc";
   --        /NOSTD_INCLUDES
   --
   --   Do not look for sources in the system default directory.

   S_Xref_Nostlib : aliased constant S := "/NOSTD_LIBRARIES "              &
                                            "-nostdlib";
   --        /NOSTD_LIBRARIES
   --
   --   Do not look for library files in the system default directory.

   S_Xref_Object  : aliased constant S := "/OBJECT_SEARCH=*"               &
                                            "-aO*";
   --        /OBJECT_SEARCH=(directory,...)
   --
   --   When searching for library and object files, look in the specified
   --   directories. The order in which library files are searched is the same
   --   as for MAKE.

   S_Xref_Project : aliased constant S := "/PROJECT=@"                     &
                                            "-p@";
   --        /PROJECT=file
   --
   --   Specify a project file to use. By default, FIND and XREF will try to
   --   locate a project file in the current directory.
   --
   --   If a project file is either specified or found by the tools, then the
   --   content of the source directory and object directory lines are added
   --   as if they had been specified respectively by /SOURCE_SEARCH and
   --   /OBJECT_SEARCH.

   S_Xref_Prj     : aliased constant S := "/PROJECT_FILE=<"                &
                                            "-P>";
   --        /PROJECT_FILE=filename
   --
   --   Specifies the main project file to be used. The project files rooted
   --   at the main project file will be parsed before doing any processing.
   --   The source and object directories to be searched will be communicated
   --   to gnatxref through logical names ADA_PRJ_INCLUDE_FILE and
   --   ADA_PRJ_OBJECTS_FILE.

   S_Xref_Search  : aliased constant S := "/SEARCH=*"                      &
                                            "-I*";
   --        /SEARCH=(directory,...)
   --
   --   Equivalent to:
   --           /OBJECT_SEARCH=(directory,...) /SOURCE_SEARCH=(directory,...)

   S_Xref_Source  : aliased constant S := "/SOURCE_SEARCH=*"               &
                                            "-aI*";
   --        /SOURCE_SEARCH=(directory,...)
   --
   --   When looking for source files also look in the specified directories.
   --   The order in which source file search is undertaken is the same as for
   --   MAKE.

   S_Xref_Subdirs : aliased constant S := "/SUBDIRS=<"                     &
                                             "--subdirs=>";
   --        /SUBDIRS=dir
   --
   --   The actual directories (object, exec, library, ...) are subdirectories
   --   of the directory specified in the project file. If the subdirectory
   --   does not exist, it is created automatically.

   S_Xref_Output  : aliased constant S := "/UNUSED "                       &
                                            "-u";
   --        /SOURCE_SEARCH=(directory,...)
   --
   --   When looking for source files also look in the specified directories.
   --   The order in which source file search is undertaken is the same as for
   --   MAKE.

   S_Xref_Tags    : aliased constant S := "/TAGS "                         &
                                            "-v";
   --        /NOTAGS (D)
   --        /TAGS
   --
   --   Print a 'tags' file for vi.

   Xref_Switches : aliased constant Switches :=
                     (S_Xref_Add     'Access,
                      S_Xref_All     'Access,
                      S_Xref_Deriv   'Access,
                      S_Xref_Ext     'Access,
                      S_Xref_Follow  'Access,
                      S_Xref_Full    'Access,
                      S_Xref_Global  'Access,
                      S_Xref_Mess    'Access,
                      S_Xref_Nostinc 'Access,
                      S_Xref_Nostlib 'Access,
                      S_Xref_Object  'Access,
                      S_Xref_Project 'Access,
                      S_Xref_Prj     'Access,
                      S_Xref_Search  'Access,
                      S_Xref_Source  'Access,
                      S_Xref_Subdirs 'Access,
                      S_Xref_Output  'Access,
                      S_Xref_Tags    'Access);

end VMS_Data;
