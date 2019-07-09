.. role:: switch(samp)

.. |with| replace:: *with*
.. |withs| replace:: *with*\ s
.. |withed| replace:: *with*\ ed
.. |withing| replace:: *with*\ ing

.. -- Example: A |withing| unit has a |with| clause, it |withs| a |withed| unit


.. _The_GNAT_Compilation_Model:

**************************
The GNAT Compilation Model
**************************

.. index:: ! GNAT compilation model

.. index:: Compilation model

This chapter describes the compilation model used by GNAT. Although
similar to that used by other languages such as C and C++, this model
is substantially different from the traditional Ada compilation models,
which are based on a centralized program library. The chapter covers
the following material:

* Topics related to source file makeup and naming

  * :ref:`Source_Representation`
  * :ref:`Foreign_Language_Representation`
  * :ref:`File_Naming_Topics_and_Utilities`

* :ref:`Configuration_Pragmas`
* :ref:`Generating_Object_Files`
* :ref:`Source_Dependencies`
* :ref:`The_Ada_Library_Information_Files`
* :ref:`Binding_an_Ada_Program`
* :ref:`GNAT_and_Libraries`
* :ref:`Conditional_Compilation`
* :ref:`Mixed_Language_Programming`
* :ref:`GNAT_and_Other_Compilation_Models`
* :ref:`Using_GNAT_Files_with_External_Tools`


.. _Source_Representation:

Source Representation
=====================

.. index:: Latin-1

.. index:: VT, HT, CR, LF, FF

Ada source programs are represented in standard text files, using
Latin-1 coding. Latin-1 is an 8-bit code that includes the familiar
7-bit ASCII set, plus additional characters used for
representing foreign languages (see :ref:`Foreign_Language_Representation`
for support of non-USA character sets). The format effector characters
are represented using their standard ASCII encodings, as follows:

    =========== ======================= ===========
     Character          Effect           Code
    ----------- ----------------------- -----------
    :kbd:`VT`    Vertical tab            ``16#0B#``
    :kbd:`HT`    Horizontal tab          ``16#09#``
    :kbd:`CR`    Carriage return         ``16#0D#``
    :kbd:`LF`    Line feed               ``16#0A#``
    :kbd:`FF`    Form feed               ``16#0C#``
    =========== ======================= ===========

Source files are in standard text file format. In addition, GNAT will
recognize a wide variety of stream formats, in which the end of
physical lines is marked by any of the following sequences:
``LF``, ``CR``, ``CR-LF``, or ``LF-CR``. This is useful
in accommodating files that are imported from other operating systems.

.. index:: pair: End of source file; Source file, end

.. index:: SUB (control character)

The end of a source file is normally represented by the physical end of
file. However, the control character ``16#1A#`` (:kbd:`SUB`) is also
recognized as signalling the end of the source file. Again, this is
provided for compatibility with other operating systems where this
code is used to represent the end of file.

.. index:: spec (definition), compilation (definition)

Each file contains a single Ada compilation unit, including any pragmas
associated with the unit. For example, this means you must place a
package declaration (a package *spec*) and the corresponding body in
separate files. An Ada *compilation* (which is a sequence of
compilation units) is represented using a sequence of files. Similarly,
you will place each subunit or child unit in a separate file.

.. _Foreign_Language_Representation:

Foreign Language Representation
===============================

GNAT supports the standard character sets defined in Ada as well as
several other non-standard character sets for use in localized versions
of the compiler (:ref:`Character_Set_Control`).

.. _Latin-1:

Latin-1
-------

.. index:: Latin-1

The basic character set is Latin-1. This character set is defined by ISO
standard 8859, part 1. The lower half (character codes ``16#00#``
... ``16#7F#)`` is identical to standard ASCII coding, but the upper
half is used to represent additional characters. These include extended letters
used by European languages, such as French accents, the vowels with umlauts
used in German, and the extra letter A-ring used in Swedish.

.. index:: Ada.Characters.Latin_1

For a complete list of Latin-1 codes and their encodings, see the source
file of library unit ``Ada.Characters.Latin_1`` in file
:file:`a-chlat1.ads`.
You may use any of these extended characters freely in character or
string literals. In addition, the extended characters that represent
letters can be used in identifiers.

.. _Other_8-Bit_Codes:

Other 8-Bit Codes
-----------------

GNAT also supports several other 8-bit coding schemes:


.. index:: Latin-2
.. index:: ISO 8859-2

*ISO 8859-2 (Latin-2)*
  Latin-2 letters allowed in identifiers, with uppercase and lowercase
  equivalence.

.. index:: Latin-3
.. index:: ISO 8859-3

*ISO 8859-3 (Latin-3)*
  Latin-3 letters allowed in identifiers, with uppercase and lowercase
  equivalence.


.. index:: Latin-4
.. index:: ISO 8859-4

*ISO 8859-4 (Latin-4)*
  Latin-4 letters allowed in identifiers, with uppercase and lowercase
  equivalence.


.. index:: ISO 8859-5
.. index:: Cyrillic

*ISO 8859-5 (Cyrillic)*
  ISO 8859-5 letters (Cyrillic) allowed in identifiers, with uppercase and
  lowercase equivalence.

.. index:: ISO 8859-15
.. index:: Latin-9

*ISO 8859-15 (Latin-9)*
  ISO 8859-15 (Latin-9) letters allowed in identifiers, with uppercase and
  lowercase equivalence

.. index:: code page 437 (IBM PC)

*IBM PC (code page 437)*
  This code page is the normal default for PCs in the U.S. It corresponds
  to the original IBM PC character set. This set has some, but not all, of
  the extended Latin-1 letters, but these letters do not have the same
  encoding as Latin-1. In this mode, these letters are allowed in
  identifiers with uppercase and lowercase equivalence.

.. index:: code page 850 (IBM PC)

*IBM PC (code page 850)*
  This code page is a modification of 437 extended to include all the
  Latin-1 letters, but still not with the usual Latin-1 encoding. In this
  mode, all these letters are allowed in identifiers with uppercase and
  lowercase equivalence.


*Full Upper 8-bit*
  Any character in the range 80-FF allowed in identifiers, and all are
  considered distinct. In other words, there are no uppercase and lowercase
  equivalences in this range. This is useful in conjunction with
  certain encoding schemes used for some foreign character sets (e.g.,
  the typical method of representing Chinese characters on the PC).


*No Upper-Half*
  No upper-half characters in the range 80-FF are allowed in identifiers.
  This gives Ada 83 compatibility for identifier names.

For precise data on the encodings permitted, and the uppercase and lowercase
equivalences that are recognized, see the file :file:`csets.adb` in
the GNAT compiler sources. You will need to obtain a full source release
of GNAT to obtain this file.

.. _Wide_Character_Encodings:

Wide_Character Encodings
------------------------

GNAT allows wide character codes to appear in character and string
literals, and also optionally in identifiers, by means of the following
possible encoding schemes:

*Hex Coding*
  In this encoding, a wide character is represented by the following five
  character sequence::

    ESC a b c d

  where ``a``, ``b``, ``c``, ``d`` are the four hexadecimal
  characters (using uppercase letters) of the wide character code. For
  example, ESC A345 is used to represent the wide character with code
  ``16#A345#``.
  This scheme is compatible with use of the full Wide_Character set.

*Upper-Half Coding*
  .. index:: Upper-Half Coding

  The wide character with encoding ``16#abcd#`` where the upper bit is on
  (in other words, 'a' is in the range 8-F) is represented as two bytes,
  ``16#ab#`` and ``16#cd#``. The second byte cannot be a format control
  character, but is not required to be in the upper half. This method can
  be also used for shift-JIS or EUC, where the internal coding matches the
  external coding.

*Shift JIS Coding*
  .. index:: Shift JIS Coding

  A wide character is represented by a two-character sequence,
  ``16#ab#`` and
  ``16#cd#``, with the restrictions described for upper-half encoding as
  described above. The internal character code is the corresponding JIS
  character according to the standard algorithm for Shift-JIS
  conversion. Only characters defined in the JIS code set table can be
  used with this encoding method.


*EUC Coding*
  .. index:: EUC Coding

  A wide character is represented by a two-character sequence
  ``16#ab#`` and
  ``16#cd#``, with both characters being in the upper half. The internal
  character code is the corresponding JIS character according to the EUC
  encoding algorithm. Only characters defined in the JIS code set table
  can be used with this encoding method.


*UTF-8 Coding*
  A wide character is represented using
  UCS Transformation Format 8 (UTF-8) as defined in Annex R of ISO
  10646-1/Am.2. Depending on the character value, the representation
  is a one, two, or three byte sequence::

    16#0000#-16#007f#: 2#0xxxxxxx#
    16#0080#-16#07ff#: 2#110xxxxx# 2#10xxxxxx#
    16#0800#-16#ffff#: 2#1110xxxx# 2#10xxxxxx# 2#10xxxxxx#

  where the ``xxx`` bits correspond to the left-padded bits of the
  16-bit character value. Note that all lower half ASCII characters
  are represented as ASCII bytes and all upper half characters and
  other wide characters are represented as sequences of upper-half
  (The full UTF-8 scheme allows for encoding 31-bit characters as
  6-byte sequences, and in the following section on wide wide
  characters, the use of these sequences is documented).


*Brackets Coding*
  In this encoding, a wide character is represented by the following eight
  character sequence::

    [ " a b c d " ]

  where ``a``, ``b``, ``c``, ``d`` are the four hexadecimal
  characters (using uppercase letters) of the wide character code. For
  example, ['A345'] is used to represent the wide character with code
  ``16#A345#``. It is also possible (though not required) to use the
  Brackets coding for upper half characters. For example, the code
  ``16#A3#`` can be represented as ``['A3']``.

  This scheme is compatible with use of the full Wide_Character set,
  and is also the method used for wide character encoding in some standard
  ACATS (Ada Conformity Assessment Test Suite) test suite distributions.

.. note::

  Some of these coding schemes do not permit the full use of the
  Ada character set. For example, neither Shift JIS nor EUC allow the
  use of the upper half of the Latin-1 set.

.. _Wide_Wide_Character_Encodings:

Wide_Wide_Character Encodings
-----------------------------

GNAT allows wide wide character codes to appear in character and string
literals, and also optionally in identifiers, by means of the following
possible encoding schemes:

*UTF-8 Coding*
  A wide character is represented using
  UCS Transformation Format 8 (UTF-8) as defined in Annex R of ISO
  10646-1/Am.2. Depending on the character value, the representation
  of character codes with values greater than 16#FFFF# is a
  is a four, five, or six byte sequence::

    16#01_0000#-16#10_FFFF#:     11110xxx 10xxxxxx 10xxxxxx
                                 10xxxxxx
    16#0020_0000#-16#03FF_FFFF#: 111110xx 10xxxxxx 10xxxxxx
                                 10xxxxxx 10xxxxxx
    16#0400_0000#-16#7FFF_FFFF#: 1111110x 10xxxxxx 10xxxxxx
                                 10xxxxxx 10xxxxxx 10xxxxxx


  where the ``xxx`` bits correspond to the left-padded bits of the
  32-bit character value.

*Brackets Coding*
  In this encoding, a wide wide character is represented by the following ten or
  twelve byte character sequence::

    [ " a b c d e f " ]
    [ " a b c d e f g h " ]

  where ``a-h`` are the six or eight hexadecimal
  characters (using uppercase letters) of the wide wide character code. For
  example, ["1F4567"] is used to represent the wide wide character with code
  ``16#001F_4567#``.

  This scheme is compatible with use of the full Wide_Wide_Character set,
  and is also the method used for wide wide character encoding in some standard
  ACATS (Ada Conformity Assessment Test Suite) test suite distributions.


.. _File_Naming_Topics_and_Utilities:

File Naming Topics and Utilities
================================

GNAT has a default file naming scheme and also provides the user with
a high degree of control over how the names and extensions of the
source files correspond to the Ada compilation units that they contain.


.. _File_Naming_Rules:

File Naming Rules
-----------------

The default file name is determined by the name of the unit that the
file contains. The name is formed by taking the full expanded name of
the unit and replacing the separating dots with hyphens and using
lowercase for all letters.

An exception arises if the file name generated by the above rules starts
with one of the characters
``a``, ``g``, ``i``, or ``s``, and the second character is a
minus. In this case, the character tilde is used in place
of the minus. The reason for this special rule is to avoid clashes with
the standard names for child units of the packages System, Ada,
Interfaces, and GNAT, which use the prefixes
``s-``, ``a-``, ``i-``, and ``g-``,
respectively.

The file extension is :file:`.ads` for a spec and
:file:`.adb` for a body. The following table shows some
examples of these rules.

   ============================ ===============================
   Source File                   Ada Compilation Unit
   ---------------------------- -------------------------------
   :file:`main.ads`              Main (spec)
   :file:`main.adb`              Main (body)
   :file:`arith_functions.ads`   Arith_Functions (package spec)
   :file:`arith_functions.adb`   Arith_Functions (package body)
   :file:`func-spec.ads`         Func.Spec (child package spec)
   :file:`func-spec.adb`         Func.Spec (child package body)
   :file:`main-sub.adb`          Sub (subunit of Main)
   :file:`a~bad.adb`             A.Bad (child package body)
   ============================ ===============================

Following these rules can result in excessively long
file names if corresponding
unit names are long (for example, if child units or subunits are
heavily nested). An option is available to shorten such long file names
(called file name 'krunching'). This may be particularly useful when
programs being developed with GNAT are to be used on operating systems
with limited file name lengths. :ref:`Using_gnatkr`.

Of course, no file shortening algorithm can guarantee uniqueness over
all possible unit names; if file name krunching is used, it is your
responsibility to ensure no name clashes occur. Alternatively you
can specify the exact file names that you want used, as described
in the next section. Finally, if your Ada programs are migrating from a
compiler with a different naming convention, you can use the gnatchop
utility to produce source files that follow the GNAT naming conventions.
(For details see :ref:`Renaming_Files_with_gnatchop`.)

Note: in the case of Windows or Mac OS operating systems, case is not
significant. So for example on Windows if the canonical name is
:file:`main-sub.adb`, you can use the file name :file:`Main-Sub.adb` instead.
However, case is significant for other operating systems, so for example,
if you want to use other than canonically cased file names on a Unix system,
you need to follow the procedures described in the next section.

.. _Using_Other_File_Names:

Using Other File Names
----------------------

.. index:: File names

In the previous section, we have described the default rules used by
GNAT to determine the file name in which a given unit resides. It is
often convenient to follow these default rules, and if you follow them,
the compiler knows without being explicitly told where to find all
the files it needs.

.. index:: Source_File_Name pragma

However, in some cases, particularly when a program is imported from
another Ada compiler environment, it may be more convenient for the
programmer to specify which file names contain which units. GNAT allows
arbitrary file names to be used by means of the Source_File_Name pragma.
The form of this pragma is as shown in the following examples:

.. code-block:: ada

      pragma Source_File_Name (My_Utilities.Stacks,
        Spec_File_Name => "myutilst_a.ada");
      pragma Source_File_name (My_Utilities.Stacks,
        Body_File_Name => "myutilst.ada");

As shown in this example, the first argument for the pragma is the unit
name (in this example a child unit). The second argument has the form
of a named association. The identifier
indicates whether the file name is for a spec or a body;
the file name itself is given by a string literal.

The source file name pragma is a configuration pragma, which means that
normally it will be placed in the :file:`gnat.adc`
file used to hold configuration
pragmas that apply to a complete compilation environment.
For more details on how the :file:`gnat.adc` file is created and used
see :ref:`Handling_of_Configuration_Pragmas`.

.. index:: gnat.adc

GNAT allows completely arbitrary file names to be specified using the
source file name pragma. However, if the file name specified has an
extension other than :file:`.ads` or :file:`.adb` it is necessary to use
a special syntax when compiling the file. The name in this case must be
preceded by the special sequence ``-x`` followed by a space and the name
of the language, here ``ada``, as in:

.. code-block:: sh

     $ gcc -c -x ada peculiar_file_name.sim

``gnatmake`` handles non-standard file names in the usual manner (the
non-standard file name for the main program is simply used as the
argument to gnatmake). Note that if the extension is also non-standard,
then it must be included in the ``gnatmake`` command, it may not
be omitted.

.. _Alternative_File_Naming_Schemes:

Alternative File Naming Schemes
-------------------------------

.. index:: File naming schemes, alternative

.. index:: File names

The previous section described the use of the ``Source_File_Name``
pragma to allow arbitrary names to be assigned to individual source files.
However, this approach requires one pragma for each file, and especially in
large systems can result in very long :file:`gnat.adc` files, and also create
a maintenance problem.

.. index:: Source_File_Name pragma

GNAT also provides a facility for specifying systematic file naming schemes
other than the standard default naming scheme previously described. An
alternative scheme for naming is specified by the use of
``Source_File_Name`` pragmas having the following format:

.. code-block:: ada

     pragma Source_File_Name (
        Spec_File_Name  => FILE_NAME_PATTERN
      [ , Casing          => CASING_SPEC]
      [ , Dot_Replacement => STRING_LITERAL ] );

     pragma Source_File_Name (
        Body_File_Name  => FILE_NAME_PATTERN
      [ , Casing          => CASING_SPEC ]
      [ , Dot_Replacement => STRING_LITERAL ] ) ;

     pragma Source_File_Name (
        Subunit_File_Name  => FILE_NAME_PATTERN
      [ , Casing          => CASING_SPEC ]
      [ , Dot_Replacement => STRING_LITERAL ] ) ;

     FILE_NAME_PATTERN ::= STRING_LITERAL
     CASING_SPEC ::= Lowercase | Uppercase | Mixedcase

The ``FILE_NAME_PATTERN`` string shows how the file name is constructed.
It contains a single asterisk character, and the unit name is substituted
systematically for this asterisk. The optional parameter
``Casing`` indicates
whether the unit name is to be all upper-case letters, all lower-case letters,
or mixed-case. If no
``Casing`` parameter is used, then the default is all
lower-case.

The optional ``Dot_Replacement`` string is used to replace any periods
that occur in subunit or child unit names. If no ``Dot_Replacement``
argument is used then separating dots appear unchanged in the resulting
file name.
Although the above syntax indicates that the
``Casing`` argument must appear
before the ``Dot_Replacement`` argument, but it
is also permissible to write these arguments in the opposite order.

As indicated, it is possible to specify different naming schemes for
bodies, specs, and subunits. Quite often the rule for subunits is the
same as the rule for bodies, in which case, there is no need to give
a separate ``Subunit_File_Name`` rule, and in this case the
``Body_File_name`` rule is used for subunits as well.

The separate rule for subunits can also be used to implement the rather
unusual case of a compilation environment (e.g., a single directory) which
contains a subunit and a child unit with the same unit name. Although
both units cannot appear in the same partition, the Ada Reference Manual
allows (but does not require) the possibility of the two units coexisting
in the same environment.

The file name translation works in the following steps:

* If there is a specific ``Source_File_Name`` pragma for the given unit,
  then this is always used, and any general pattern rules are ignored.

* If there is a pattern type ``Source_File_Name`` pragma that applies to
  the unit, then the resulting file name will be used if the file exists. If
  more than one pattern matches, the latest one will be tried first, and the
  first attempt resulting in a reference to a file that exists will be used.

* If no pattern type ``Source_File_Name`` pragma that applies to the unit
  for which the corresponding file exists, then the standard GNAT default
  naming rules are used.

As an example of the use of this mechanism, consider a commonly used scheme
in which file names are all lower case, with separating periods copied
unchanged to the resulting file name, and specs end with :file:`.1.ada`, and
bodies end with :file:`.2.ada`. GNAT will follow this scheme if the following
two pragmas appear:

.. code-block:: ada

     pragma Source_File_Name
       (Spec_File_Name => ".1.ada");
     pragma Source_File_Name
       (Body_File_Name => ".2.ada");

The default GNAT scheme is actually implemented by providing the following
default pragmas internally:

.. code-block:: ada

     pragma Source_File_Name
       (Spec_File_Name => ".ads", Dot_Replacement => "-");
     pragma Source_File_Name
       (Body_File_Name => ".adb", Dot_Replacement => "-");

Our final example implements a scheme typically used with one of the
Ada 83 compilers, where the separator character for subunits was '__'
(two underscores), specs were identified by adding :file:`_.ADA`, bodies
by adding :file:`.ADA`, and subunits by
adding :file:`.SEP`. All file names were
upper case. Child units were not present of course since this was an
Ada 83 compiler, but it seems reasonable to extend this scheme to use
the same double underscore separator for child units.

.. code-block:: ada

     pragma Source_File_Name
       (Spec_File_Name => "_.ADA",
        Dot_Replacement => "__",
        Casing = Uppercase);
     pragma Source_File_Name
       (Body_File_Name => ".ADA",
        Dot_Replacement => "__",
        Casing = Uppercase);
     pragma Source_File_Name
       (Subunit_File_Name => ".SEP",
        Dot_Replacement => "__",
        Casing = Uppercase);


.. index:: ! gnatname

.. _Handling_Arbitrary_File_Naming_Conventions_with_gnatname:

Handling Arbitrary File Naming Conventions with ``gnatname``
------------------------------------------------------------

.. index:: File Naming Conventions

.. _Arbitrary_File_Naming_Conventions:

Arbitrary File Naming Conventions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The GNAT compiler must be able to know the source file name of a compilation
unit.  When using the standard GNAT default file naming conventions
(``.ads`` for specs, ``.adb`` for bodies), the GNAT compiler
does not need additional information.

When the source file names do not follow the standard GNAT default file naming
conventions, the GNAT compiler must be given additional information through
a configuration pragmas file (:ref:`Configuration_Pragmas`)
or a project file.
When the non-standard file naming conventions are well-defined,
a small number of pragmas ``Source_File_Name`` specifying a naming pattern
(:ref:`Alternative_File_Naming_Schemes`) may be sufficient. However,
if the file naming conventions are irregular or arbitrary, a number
of pragma ``Source_File_Name`` for individual compilation units
must be defined.
To help maintain the correspondence between compilation unit names and
source file names within the compiler,
GNAT provides a tool ``gnatname`` to generate the required pragmas for a
set of files.

.. _Running_gnatname:

Running ``gnatname``
^^^^^^^^^^^^^^^^^^^^

The usual form of the ``gnatname`` command is:

.. code-block:: sh

      $ gnatname [ switches ]  naming_pattern  [ naming_patterns ]
          [--and [ switches ]  naming_pattern  [ naming_patterns ]]


All of the arguments are optional. If invoked without any argument,
``gnatname`` will display its usage.

When used with at least one naming pattern, ``gnatname`` will attempt to
find all the compilation units in files that follow at least one of the
naming patterns. To find these compilation units,
``gnatname`` will use the GNAT compiler in syntax-check-only mode on all
regular files.

One or several Naming Patterns may be given as arguments to ``gnatname``.
Each Naming Pattern is enclosed between double quotes (or single
quotes on Windows).
A Naming Pattern is a regular expression similar to the wildcard patterns
used in file names by the Unix shells or the DOS prompt.

``gnatname`` may be called with several sections of directories/patterns.
Sections are separated by the switch :switch:`--and`. In each section, there must be
at least one pattern. If no directory is specified in a section, the current
directory (or the project directory if :switch:`-P` is used) is implied.
The options other that the directory switches and the patterns apply globally
even if they are in different sections.

Examples of Naming Patterns are::

     "*.[12].ada"
     "*.ad[sb]*"
     "body_*"    "spec_*"

For a more complete description of the syntax of Naming Patterns,
see the second kind of regular expressions described in :file:`g-regexp.ads`
(the 'Glob' regular expressions).

When invoked without the switch :switch:`-P`, ``gnatname`` will create a
configuration pragmas file :file:`gnat.adc` in the current working directory,
with pragmas ``Source_File_Name`` for each file that contains a valid Ada
unit.

.. _Switches_for_gnatname:

Switches for ``gnatname``
^^^^^^^^^^^^^^^^^^^^^^^^^

Switches for ``gnatname`` must precede any specified Naming Pattern.

You may specify any of the following switches to ``gnatname``:

.. index:: --version (gnatname)

:switch:`--version`
  Display Copyright and version, then exit disregarding all other options.

.. index:: --help (gnatname)

:switch:`--help`
  If :switch:`--version` was not used, display usage, then exit disregarding
  all other options.

:switch:`--subdirs={dir}`
  Real object, library or exec directories are subdirectories <dir> of the
  specified ones.

:switch:`--no-backup`
  Do not create a backup copy of an existing project file.

:switch:`--and`
  Start another section of directories/patterns.

.. index:: -c (gnatname)

:switch:`-c{filename}`
  Create a configuration pragmas file :file:`filename` (instead of the default
  :file:`gnat.adc`).
  There may be zero, one or more space between :switch:`-c` and
  :file:`filename`.
  :file:`filename` may include directory information. :file:`filename` must be
  writable. There may be only one switch :switch:`-c`.
  When a switch :switch:`-c` is
  specified, no switch :switch:`-P` may be specified (see below).

.. index:: -d (gnatname)

:switch:`-d{dir}`
  Look for source files in directory :file:`dir`. There may be zero, one or more
  spaces between :switch:`-d` and :file:`dir`.
  :file:`dir` may end with ``/**``, that is it may be of the form
  ``root_dir/**``. In this case, the directory ``root_dir`` and all of its
  subdirectories, recursively, have to be searched for sources.
  When a switch :switch:`-d`
  is specified, the current working directory will not be searched for source
  files, unless it is explicitly specified with a :switch:`-d`
  or :switch:`-D` switch.
  Several switches :switch:`-d` may be specified.
  If :file:`dir` is a relative path, it is relative to the directory of
  the configuration pragmas file specified with switch
  :switch:`-c`,
  or to the directory of the project file specified with switch
  :switch:`-P` or,
  if neither switch :switch:`-c`
  nor switch :switch:`-P` are specified, it is relative to the
  current working directory. The directory
  specified with switch :switch:`-d` must exist and be readable.

.. index:: -D (gnatname)

:switch:`-D{filename}`
  Look for source files in all directories listed in text file :file:`filename`.
  There may be zero, one or more spaces between :switch:`-D`
  and :file:`filename`.
  :file:`filename` must be an existing, readable text file.
  Each nonempty line in :file:`filename` must be a directory.
  Specifying switch :switch:`-D` is equivalent to specifying as many
  switches :switch:`-d` as there are nonempty lines in
  :file:`file`.

:switch:`-eL`
  Follow symbolic links when processing project files.

  .. index:: -f (gnatname)

:switch:`-f{pattern}`
  Foreign patterns. Using this switch, it is possible to add sources of languages
  other than Ada to the list of sources of a project file.
  It is only useful if a -P switch is used.
  For example,

  .. code-block:: sh

     gnatname -Pprj -f"*.c" "*.ada"

  will look for Ada units in all files with the :file:`.ada` extension,
  and will add to the list of file for project :file:`prj.gpr` the C files
  with extension :file:`.c`.

  .. index:: -h (gnatname)

:switch:`-h`
  Output usage (help) information. The output is written to :file:`stdout`.

  .. index:: -P (gnatname)

:switch:`-P{proj}`
  Create or update project file :file:`proj`. There may be zero, one or more space
  between :switch:`-P` and :file:`proj`. :file:`proj` may include directory
  information. :file:`proj` must be writable.
  There may be only one switch :switch:`-P`.
  When a switch :switch:`-P` is specified,
  no switch :switch:`-c` may be specified.
  On all platforms, except on VMS, when ``gnatname`` is invoked for an
  existing project file <proj>.gpr, a backup copy of the project file is created
  in the project directory with file name <proj>.gpr.saved_x. 'x' is the first
  non negative number that makes this backup copy a new file.

  .. index:: -v (gnatname)

:switch:`-v`
  Verbose mode. Output detailed explanation of behavior to :file:`stdout`.
  This includes name of the file written, the name of the directories to search
  and, for each file in those directories whose name matches at least one of
  the Naming Patterns, an indication of whether the file contains a unit,
  and if so the name of the unit.

.. index:: -v -v (gnatname)

:switch:`-v -v`
  Very Verbose mode. In addition to the output produced in verbose mode,
  for each file in the searched directories whose name matches none of
  the Naming Patterns, an indication is given that there is no match.

  .. index:: -x (gnatname)

:switch:`-x{pattern}`
  Excluded patterns. Using this switch, it is possible to exclude some files
  that would match the name patterns. For example,

  .. code-block:: sh

      gnatname -x "*_nt.ada" "*.ada"

  will look for Ada units in all files with the :file:`.ada` extension,
  except those whose names end with :file:`_nt.ada`.


.. _Examples_of_gnatname_Usage:

Examples of ``gnatname`` Usage
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: sh

     $ gnatname -c /home/me/names.adc -d sources "[a-z]*.ada*"

In this example, the directory :file:`/home/me` must already exist
and be writable. In addition, the directory
:file:`/home/me/sources` (specified by
:switch:`-d sources`) must exist and be readable.

Note the optional spaces after :switch:`-c` and :switch:`-d`.

.. code-block:: sh

     $ gnatname -P/home/me/proj -x "*_nt_body.ada"
     -dsources -dsources/plus -Dcommon_dirs.txt "body_*" "spec_*"

Note that several switches :switch:`-d` may be used,
even in conjunction with one or several switches
:switch:`-D`. Several Naming Patterns and one excluded pattern
are used in this example.


.. _File_Name_Krunching_with_gnatkr:

File Name Krunching with ``gnatkr``
-----------------------------------

.. index:: ! gnatkr

This section discusses the method used by the compiler to shorten
the default file names chosen for Ada units so that they do not
exceed the maximum length permitted. It also describes the
``gnatkr`` utility that can be used to determine the result of
applying this shortening.

.. _About_gnatkr:

About ``gnatkr``
^^^^^^^^^^^^^^^^

The default file naming rule in GNAT
is that the file name must be derived from
the unit name. The exact default rule is as follows:

* Take the unit name and replace all dots by hyphens.

* If such a replacement occurs in the
  second character position of a name, and the first character is
  :samp:`a`, :samp:`g`, :samp:`s`, or :samp:`i`,
  then replace the dot by the character
  :samp:`~` (tilde)
  instead of a minus.

  The reason for this exception is to avoid clashes
  with the standard names for children of System, Ada, Interfaces,
  and GNAT, which use the prefixes
  :samp:`s-`, :samp:`a-`, :samp:`i-`, and :samp:`g-`,
  respectively.

The :switch:`-gnatk{nn}`
switch of the compiler activates a 'krunching'
circuit that limits file names to nn characters (where nn is a decimal
integer).

The ``gnatkr`` utility can be used to determine the krunched name for
a given file, when krunched to a specified maximum length.

.. _Using_gnatkr:

Using ``gnatkr``
^^^^^^^^^^^^^^^^

The ``gnatkr`` command has the form:

.. code-block:: sh

      $ gnatkr name [ length ]

``name`` is the uncrunched file name, derived from the name of the unit
in the standard manner described in the previous section (i.e., in particular
all dots are replaced by hyphens). The file name may or may not have an
extension (defined as a suffix of the form period followed by arbitrary
characters other than period). If an extension is present then it will
be preserved in the output. For example, when krunching :file:`hellofile.ads`
to eight characters, the result will be hellofil.ads.

Note: for compatibility with previous versions of ``gnatkr`` dots may
appear in the name instead of hyphens, but the last dot will always be
taken as the start of an extension. So if ``gnatkr`` is given an argument
such as :file:`Hello.World.adb` it will be treated exactly as if the first
period had been a hyphen, and for example krunching to eight characters
gives the result :file:`hellworl.adb`.

Note that the result is always all lower case.
Characters of the other case are folded as required.

``length`` represents the length of the krunched name. The default
when no argument is given is 8 characters. A length of zero stands for
unlimited, in other words do not chop except for system files where the
implied crunching length is always eight characters.

The output is the krunched name. The output has an extension only if the
original argument was a file name with an extension.

.. _Krunching_Method:

Krunching Method
^^^^^^^^^^^^^^^^

The initial file name is determined by the name of the unit that the file
contains. The name is formed by taking the full expanded name of the
unit and replacing the separating dots with hyphens and
using lowercase
for all letters, except that a hyphen in the second character position is
replaced by a tilde if the first character is
:samp:`a`, :samp:`i`, :samp:`g`, or :samp:`s`.
The extension is ``.ads`` for a
spec and ``.adb`` for a body.
Krunching does not affect the extension, but the file name is shortened to
the specified length by following these rules:

* The name is divided into segments separated by hyphens, tildes or
  underscores and all hyphens, tildes, and underscores are
  eliminated. If this leaves the name short enough, we are done.

* If the name is too long, the longest segment is located (left-most
  if there are two of equal length), and shortened by dropping
  its last character. This is repeated until the name is short enough.

  As an example, consider the krunching of :file:`our-strings-wide_fixed.adb`
  to fit the name into 8 characters as required by some operating systems::

      our-strings-wide_fixed 22
      our strings wide fixed 19
      our string  wide fixed 18
      our strin   wide fixed 17
      our stri    wide fixed 16
      our stri    wide fixe  15
      our str     wide fixe  14
      our str     wid  fixe  13
      our str     wid  fix   12
      ou  str     wid  fix   11
      ou  st      wid  fix   10
      ou  st      wi   fix   9
      ou  st      wi   fi    8
      Final file name: oustwifi.adb

* The file names for all predefined units are always krunched to eight
  characters. The krunching of these predefined units uses the following
  special prefix replacements:

  ===================== ==============
  Prefix                 Replacement
  --------------------- --------------
  :file:`ada-`           :file:`a-`
  :file:`gnat-`          :file:`g-`
  :file:`interfac es-`   :file:`i-`
  :file:`system-`        :file:`s-`
  ===================== ==============

  These system files have a hyphen in the second character position. That
  is why normal user files replace such a character with a
  tilde, to avoid confusion with system file names.

  As an example of this special rule, consider
  :file:`ada-strings-wide_fixed.adb`, which gets krunched as follows::

      ada-strings-wide_fixed 22
      a-  strings wide fixed 18
      a-  string  wide fixed 17
      a-  strin   wide fixed 16
      a-  stri    wide fixed 15
      a-  stri    wide fixe  14
      a-  str     wide fixe  13
      a-  str     wid  fixe  12
      a-  str     wid  fix   11
      a-  st      wid  fix   10
      a-  st      wi   fix   9
      a-  st      wi   fi    8
      Final file name: a-stwifi.adb

Of course no file shortening algorithm can guarantee uniqueness over all
possible unit names, and if file name krunching is used then it is your
responsibility to ensure that no name clashes occur. The utility
program ``gnatkr`` is supplied for conveniently determining the
krunched name of a file.

.. _Examples_of_gnatkr_Usage:

Examples of ``gnatkr`` Usage
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

::

    $ gnatkr very_long_unit_name.ads      --> velounna.ads
    $ gnatkr grandparent-parent-child.ads --> grparchi.ads
    $ gnatkr Grandparent.Parent.Child.ads --> grparchi.ads
    $ gnatkr grandparent-parent-child     --> grparchi
    $ gnatkr very_long_unit_name.ads/count=6 --> vlunna.ads
    $ gnatkr very_long_unit_name.ads/count=0 --> very_long_unit_name.ads


.. _Renaming_Files_with_gnatchop:

Renaming Files with ``gnatchop``
--------------------------------

.. index:: ! gnatchop

This section discusses how to handle files with multiple units by using
the ``gnatchop`` utility. This utility is also useful in renaming
files to meet the standard GNAT default file naming conventions.

.. _Handling_Files_with_Multiple_Units:

Handling Files with Multiple Units
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The basic compilation model of GNAT requires that a file submitted to the
compiler have only one unit and there be a strict correspondence
between the file name and the unit name.

The ``gnatchop`` utility allows both of these rules to be relaxed,
allowing GNAT to process files which contain multiple compilation units
and files with arbitrary file names. ``gnatchop``
reads the specified file and generates one or more output files,
containing one unit per file. The unit and the file name correspond,
as required by GNAT.

If you want to permanently restructure a set of 'foreign' files so that
they match the GNAT rules, and do the remaining development using the
GNAT structure, you can simply use ``gnatchop`` once, generate the
new set of files and work with them from that point on.

Alternatively, if you want to keep your files in the 'foreign' format,
perhaps to maintain compatibility with some other Ada compilation
system, you can set up a procedure where you use ``gnatchop`` each
time you compile, regarding the source files that it writes as temporary
files that you throw away.

Note that if your file containing multiple units starts with a byte order
mark (BOM) specifying UTF-8 encoding, then the files generated by gnatchop
will each start with a copy of this BOM, meaning that they can be compiled
automatically in UTF-8 mode without needing to specify an explicit encoding.

.. _Operating_gnatchop_in_Compilation_Mode:

Operating gnatchop in Compilation Mode
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The basic function of ``gnatchop`` is to take a file with multiple units
and split it into separate files. The boundary between files is reasonably
clear, except for the issue of comments and pragmas. In default mode, the
rule is that any pragmas between units belong to the previous unit, except
that configuration pragmas always belong to the following unit. Any comments
belong to the following unit. These rules
almost always result in the right choice of
the split point without needing to mark it explicitly and most users will
find this default to be what they want. In this default mode it is incorrect to
submit a file containing only configuration pragmas, or one that ends in
configuration pragmas, to ``gnatchop``.

However, using a special option to activate 'compilation mode',
``gnatchop``
can perform another function, which is to provide exactly the semantics
required by the RM for handling of configuration pragmas in a compilation.
In the absence of configuration pragmas (at the main file level), this
option has no effect, but it causes such configuration pragmas to be handled
in a quite different manner.

First, in compilation mode, if ``gnatchop`` is given a file that consists of
only configuration pragmas, then this file is appended to the
:file:`gnat.adc` file in the current directory. This behavior provides
the required behavior described in the RM for the actions to be taken
on submitting such a file to the compiler, namely that these pragmas
should apply to all subsequent compilations in the same compilation
environment. Using GNAT, the current directory, possibly containing a
:file:`gnat.adc` file is the representation
of a compilation environment. For more information on the
:file:`gnat.adc` file, see :ref:`Handling_of_Configuration_Pragmas`.

Second, in compilation mode, if ``gnatchop``
is given a file that starts with
configuration pragmas, and contains one or more units, then these
configuration pragmas are prepended to each of the chopped files. This
behavior provides the required behavior described in the RM for the
actions to be taken on compiling such a file, namely that the pragmas
apply to all units in the compilation, but not to subsequently compiled
units.

Finally, if configuration pragmas appear between units, they are appended
to the previous unit. This results in the previous unit being illegal,
since the compiler does not accept configuration pragmas that follow
a unit. This provides the required RM behavior that forbids configuration
pragmas other than those preceding the first compilation unit of a
compilation.

For most purposes, ``gnatchop`` will be used in default mode. The
compilation mode described above is used only if you need exactly
accurate behavior with respect to compilations, and you have files
that contain multiple units and configuration pragmas. In this
circumstance the use of ``gnatchop`` with the compilation mode
switch provides the required behavior, and is for example the mode
in which GNAT processes the ACVC tests.


.. _Command_Line_for_gnatchop:

Command Line for ``gnatchop``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The ``gnatchop`` command has the form:

.. code-block:: sh

     $ gnatchop switches file_name [file_name ...]
           [directory]

The only required argument is the file name of the file to be chopped.
There are no restrictions on the form of this file name. The file itself
contains one or more Ada units, in normal GNAT format, concatenated
together. As shown, more than one file may be presented to be chopped.

When run in default mode, ``gnatchop`` generates one output file in
the current directory for each unit in each of the files.

``directory``, if specified, gives the name of the directory to which
the output files will be written. If it is not specified, all files are
written to the current directory.

For example, given a
file called :file:`hellofiles` containing

.. code-block:: ada

     procedure Hello;

     with Ada.Text_IO; use Ada.Text_IO;
     procedure Hello is
     begin
        Put_Line ("Hello");
     end Hello;

the command

.. code-block:: sh

     $ gnatchop hellofiles

generates two files in the current directory, one called
:file:`hello.ads` containing the single line that is the procedure spec,
and the other called :file:`hello.adb` containing the remaining text. The
original file is not affected. The generated files can be compiled in
the normal manner.

When gnatchop is invoked on a file that is empty or that contains only empty
lines and/or comments, gnatchop will not fail, but will not produce any
new sources.

For example, given a
file called :file:`toto.txt` containing

.. code-block:: ada

     --  Just a comment

the command

.. code-block:: sh

     $ gnatchop toto.txt

will not produce any new file and will result in the following warnings::

     toto.txt:1:01: warning: empty file, contains no compilation units
     no compilation units found
     no source files written


.. _Switches_for_gnatchop:

Switches for ``gnatchop``
^^^^^^^^^^^^^^^^^^^^^^^^^

``gnatchop`` recognizes the following switches:


.. index:: --version (gnatchop)

:switch:`--version`
  Display Copyright and version, then exit disregarding all other options.

.. index:: --help (gnatchop)

:switch:`--help`
  If :switch:`--version` was not used, display usage, then exit disregarding
  all other options.

.. index:: -c (gnatchop)

:switch:`-c`
  Causes ``gnatchop`` to operate in compilation mode, in which
  configuration pragmas are handled according to strict RM rules. See
  previous section for a full description of this mode.

:switch:`-gnat{xxx}`
  This passes the given :switch:`-gnat{xxx}` switch to ``gnat`` which is
  used to parse the given file. Not all *xxx* options make sense,
  but for example, the use of :switch:`-gnati2` allows ``gnatchop`` to
  process a source file that uses Latin-2 coding for identifiers.

:switch:`-h`
  Causes ``gnatchop`` to generate a brief help summary to the standard
  output file showing usage information.

.. index:: -k (gnatchop)

:switch:`-k{mm}`
  Limit generated file names to the specified number ``mm``
  of characters.
  This is useful if the
  resulting set of files is required to be interoperable with systems
  which limit the length of file names.
  No space is allowed between the :switch:`-k` and the numeric value. The numeric
  value may be omitted in which case a default of :switch:`-k8`,
  suitable for use
  with DOS-like file systems, is used. If no :switch:`-k` switch
  is present then
  there is no limit on the length of file names.

.. index:: -p (gnatchop)

:switch:`-p`
  Causes the file modification time stamp of the input file to be
  preserved and used for the time stamp of the output file(s). This may be
  useful for preserving coherency of time stamps in an environment where
  ``gnatchop`` is used as part of a standard build process.

.. index:: -q (gnatchop)

:switch:`-q`
  Causes output of informational messages indicating the set of generated
  files to be suppressed. Warnings and error messages are unaffected.

.. index:: -r (gnatchop)
.. index:: Source_Reference pragmas

:switch:`-r`
  Generate ``Source_Reference`` pragmas. Use this switch if the output
  files are regarded as temporary and development is to be done in terms
  of the original unchopped file. This switch causes
  ``Source_Reference`` pragmas to be inserted into each of the
  generated files to refers back to the original file name and line number.
  The result is that all error messages refer back to the original
  unchopped file.
  In addition, the debugging information placed into the object file (when
  the :switch:`-g` switch of ``gcc`` or ``gnatmake`` is
  specified)
  also refers back to this original file so that tools like profilers and
  debuggers will give information in terms of the original unchopped file.

  If the original file to be chopped itself contains
  a ``Source_Reference``
  pragma referencing a third file, then gnatchop respects
  this pragma, and the generated ``Source_Reference`` pragmas
  in the chopped file refer to the original file, with appropriate
  line numbers. This is particularly useful when ``gnatchop``
  is used in conjunction with ``gnatprep`` to compile files that
  contain preprocessing statements and multiple units.

.. index:: -v (gnatchop)

:switch:`-v`
  Causes ``gnatchop`` to operate in verbose mode. The version
  number and copyright notice are output, as well as exact copies of
  the gnat1 commands spawned to obtain the chop control information.

.. index:: -w (gnatchop)

:switch:`-w`
  Overwrite existing file names. Normally ``gnatchop`` regards it as a
  fatal error if there is already a file with the same name as a
  file it would otherwise output, in other words if the files to be
  chopped contain duplicated units. This switch bypasses this
  check, and causes all but the last instance of such duplicated
  units to be skipped.

.. index:: --GCC= (gnatchop)

:switch:`--GCC={xxxx}`
  Specify the path of the GNAT parser to be used. When this switch is used,
  no attempt is made to add the prefix to the GNAT parser executable.


.. _Examples_of_gnatchop_Usage:

Examples of ``gnatchop`` Usage
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: sh

      $ gnatchop -w hello_s.ada prerelease/files

Chops the source file :file:`hello_s.ada`. The output files will be
placed in the directory :file:`prerelease/files`,
overwriting any
files with matching names in that directory (no files in the current
directory are modified).

.. code-block:: sh

      $ gnatchop archive

Chops the source file :file:`archive`
into the current directory. One
useful application of ``gnatchop`` is in sending sets of sources
around, for example in email messages. The required sources are simply
concatenated (for example, using a Unix ``cat``
command), and then
``gnatchop`` is used at the other end to reconstitute the original
file names.

.. code-block:: sh

      $ gnatchop file1 file2 file3 direc

Chops all units in files :file:`file1`, :file:`file2`, :file:`file3`, placing
the resulting files in the directory :file:`direc`. Note that if any units
occur more than once anywhere within this set of files, an error message
is generated, and no files are written. To override this check, use the
:switch:`-w` switch,
in which case the last occurrence in the last file will
be the one that is output, and earlier duplicate occurrences for a given
unit will be skipped.

.. _Configuration_Pragmas:

Configuration Pragmas
=====================

.. index:: Configuration pragmas

.. index:: Pragmas, configuration

Configuration pragmas include those pragmas described as
such in the Ada Reference Manual, as well as
implementation-dependent pragmas that are configuration pragmas.
See the ``Implementation_Defined_Pragmas`` chapter in the
:title:`GNAT_Reference_Manual` for details on these
additional GNAT-specific configuration pragmas.
Most notably, the pragma ``Source_File_Name``, which allows
specifying non-default names for source files, is a configuration
pragma. The following is a complete list of configuration pragmas
recognized by GNAT::

     Ada_83
     Ada_95
     Ada_05
     Ada_2005
     Ada_12
     Ada_2012
     Allow_Integer_Address
     Annotate
     Assertion_Policy
     Assume_No_Invalid_Values
     C_Pass_By_Copy
     Check_Float_Overflow
     Check_Name
     Check_Policy
     Compile_Time_Error
     Compile_Time_Warning
     Compiler_Unit
     Compiler_Unit_Warning
     Component_Alignment
     Convention_Identifier
     Debug_Policy
     Detect_Blocking
     Default_Scalar_Storage_Order
     Default_Storage_Pool
     Disable_Atomic_Synchronization
     Discard_Names
     Elaboration_Checks
     Eliminate
     Enable_Atomic_Synchronization
     Extend_System
     Extensions_Allowed
     External_Name_Casing
     Fast_Math
     Favor_Top_Level
     Ignore_Pragma
     Implicit_Packing
     Initialize_Scalars
     Interrupt_State
     License
     Locking_Policy
     No_Component_Reordering
     No_Heap_Finalization
     No_Run_Time
     No_Strict_Aliasing
     Normalize_Scalars
     Optimize_Alignment
     Overflow_Mode
     Overriding_Renamings
     Partition_Elaboration_Policy
     Persistent_BSS
     Polling
     Prefix_Exception_Messages
     Priority_Specific_Dispatching
     Profile
     Profile_Warnings
     Propagate_Exceptions
     Queuing_Policy
     Rational
     Ravenscar
     Rename_Pragma
     Restricted_Run_Time
     Restrictions
     Restrictions_Warnings
     Reviewable
     Short_Circuit_And_Or
     Short_Descriptors
     Source_File_Name
     Source_File_Name_Project
     SPARK_Mode
     Style_Checks
     Suppress
     Suppress_Exception_Locations
     Task_Dispatching_Policy
     Unevaluated_Use_Of_Old
     Universal_Data
     Unsuppress
     Use_VADS_Size
     Validity_Checks
     Warning_As_Error
     Warnings
     Wide_Character_Encoding


.. _Handling_of_Configuration_Pragmas:

Handling of Configuration Pragmas
---------------------------------

Configuration pragmas may either appear at the start of a compilation
unit, or they can appear in a configuration pragma file to apply to
all compilations performed in a given compilation environment.

GNAT also provides the ``gnatchop`` utility to provide an automatic
way to handle configuration pragmas following the semantics for
compilations (that is, files with multiple units), described in the RM.
See :ref:`Operating_gnatchop_in_Compilation_Mode` for details.
However, for most purposes, it will be more convenient to edit the
:file:`gnat.adc` file that contains configuration pragmas directly,
as described in the following section.

In the case of ``Restrictions`` pragmas appearing as configuration
pragmas in individual compilation units, the exact handling depends on
the type of restriction.

Restrictions that require partition-wide consistency (like
``No_Tasking``) are
recognized wherever they appear
and can be freely inherited, e.g. from a |withed| unit to the |withing|
unit. This makes sense since the binder will in any case insist on seeing
consistent use, so any unit not conforming to any restrictions that are
anywhere in the partition will be rejected, and you might as well find
that out at compile time rather than at bind time.

For restrictions that do not require partition-wide consistency, e.g.
SPARK or No_Implementation_Attributes, in general the restriction applies
only to the unit in which the pragma appears, and not to any other units.

The exception is No_Elaboration_Code which always applies to the entire
object file from a compilation, i.e. to the body, spec, and all subunits.
This restriction can be specified in a configuration pragma file, or it
can be on the body and/or the spec (in eithe case it applies to all the
relevant units). It can appear on a subunit only if it has previously
appeared in the body of spec.


.. _The_Configuration_Pragmas_Files:

The Configuration Pragmas Files
-------------------------------

.. index:: gnat.adc

In GNAT a compilation environment is defined by the current
directory at the time that a compile command is given. This current
directory is searched for a file whose name is :file:`gnat.adc`. If
this file is present, it is expected to contain one or more
configuration pragmas that will be applied to the current compilation.
However, if the switch :switch:`-gnatA` is used, :file:`gnat.adc` is not
considered. When taken into account, :file:`gnat.adc` is added to the
dependencies, so that if :file:`gnat.adc` is modified later, an invocation of
``gnatmake`` will recompile the source.

Configuration pragmas may be entered into the :file:`gnat.adc` file
either by running ``gnatchop`` on a source file that consists only of
configuration pragmas, or more conveniently by direct editing of the
:file:`gnat.adc` file, which is a standard format source file.

Besides :file:`gnat.adc`, additional files containing configuration
pragmas may be applied to the current compilation using the switch
:switch:`-gnatec={path}` where ``path`` must designate an existing file that
contains only configuration pragmas. These configuration pragmas are
in addition to those found in :file:`gnat.adc` (provided :file:`gnat.adc`
is present and switch :switch:`-gnatA` is not used).

It is allowable to specify several switches :switch:`-gnatec=`, all of which
will be taken into account.

Files containing configuration pragmas specified with switches
:switch:`-gnatec=` are added to the dependencies, unless they are
temporary files. A file is considered temporary if its name ends in
:file:`.tmp` or :file:`.TMP`. Certain tools follow this naming
convention because they pass information to ``gcc`` via
temporary files that are immediately deleted; it doesn't make sense to
depend on a file that no longer exists. Such tools include
``gprbuild``, ``gnatmake``, and ``gnatcheck``.

If you are using project file, a separate mechanism is provided using
project attributes.

.. --Comment
   See :ref:`Specifying_Configuration_Pragmas` for more details.


.. _Generating_Object_Files:

Generating Object Files
=======================

An Ada program consists of a set of source files, and the first step in
compiling the program is to generate the corresponding object files.
These are generated by compiling a subset of these source files.
The files you need to compile are the following:

* If a package spec has no body, compile the package spec to produce the
  object file for the package.

* If a package has both a spec and a body, compile the body to produce the
  object file for the package. The source file for the package spec need
  not be compiled in this case because there is only one object file, which
  contains the code for both the spec and body of the package.

* For a subprogram, compile the subprogram body to produce the object file
  for the subprogram. The spec, if one is present, is as usual in a
  separate file, and need not be compiled.

.. index:: Subunits

* In the case of subunits, only compile the parent unit. A single object
  file is generated for the entire subunit tree, which includes all the
  subunits.

* Compile child units independently of their parent units
  (though, of course, the spec of all the ancestor unit must be present in order
  to compile a child unit).

  .. index:: Generics

* Compile generic units in the same manner as any other units. The object
  files in this case are small dummy files that contain at most the
  flag used for elaboration checking. This is because GNAT always handles generic
  instantiation by means of macro expansion. However, it is still necessary to
  compile generic units, for dependency checking and elaboration purposes.

The preceding rules describe the set of files that must be compiled to
generate the object files for a program. Each object file has the same
name as the corresponding source file, except that the extension is
:file:`.o` as usual.

You may wish to compile other files for the purpose of checking their
syntactic and semantic correctness. For example, in the case where a
package has a separate spec and body, you would not normally compile the
spec. However, it is convenient in practice to compile the spec to make
sure it is error-free before compiling clients of this spec, because such
compilations will fail if there is an error in the spec.

GNAT provides an option for compiling such files purely for the
purposes of checking correctness; such compilations are not required as
part of the process of building a program. To compile a file in this
checking mode, use the :switch:`-gnatc` switch.

.. _Source_Dependencies:

Source Dependencies
===================

A given object file clearly depends on the source file which is compiled
to produce it. Here we are using "depends" in the sense of a typical
``make`` utility; in other words, an object file depends on a source
file if changes to the source file require the object file to be
recompiled.
In addition to this basic dependency, a given object may depend on
additional source files as follows:

* If a file being compiled |withs| a unit ``X``, the object file
  depends on the file containing the spec of unit ``X``. This includes
  files that are |withed| implicitly either because they are parents
  of |withed| child units or they are run-time units required by the
  language constructs used in a particular unit.

* If a file being compiled instantiates a library level generic unit, the
  object file depends on both the spec and body files for this generic
  unit.

* If a file being compiled instantiates a generic unit defined within a
  package, the object file depends on the body file for the package as
  well as the spec file.

.. index:: Inline
.. index:: -gnatn switch

* If a file being compiled contains a call to a subprogram for which
  pragma ``Inline`` applies and inlining is activated with the
  :switch:`-gnatn` switch, the object file depends on the file containing the
  body of this subprogram as well as on the file containing the spec. Note
  that for inlining to actually occur as a result of the use of this switch,
  it is necessary to compile in optimizing mode.

  .. index:: -gnatN switch

  The use of :switch:`-gnatN` activates  inlining optimization
  that is performed by the front end of the compiler. This inlining does
  not require that the code generation be optimized. Like :switch:`-gnatn`,
  the use of this switch generates additional dependencies.

  When using a gcc-based back end (in practice this means using any version
  of GNAT other than for the JVM, .NET or GNAAMP platforms), then the use of
  :switch:`-gnatN` is deprecated, and the use of :switch:`-gnatn` is preferred.
  Historically front end inlining was more extensive than the gcc back end
  inlining, but that is no longer the case.

* If an object file :file:`O` depends on the proper body of a subunit through
  inlining or instantiation, it depends on the parent unit of the subunit.
  This means that any modification of the parent unit or one of its subunits
  affects the compilation of :file:`O`.

* The object file for a parent unit depends on all its subunit body files.

* The previous two rules meant that for purposes of computing dependencies and
  recompilation, a body and all its subunits are treated as an indivisible whole.

  These rules are applied transitively: if unit ``A`` |withs|
  unit ``B``, whose elaboration calls an inlined procedure in package
  ``C``, the object file for unit ``A`` will depend on the body of
  ``C``, in file :file:`c.adb`.

  The set of dependent files described by these rules includes all the
  files on which the unit is semantically dependent, as dictated by the
  Ada language standard. However, it is a superset of what the
  standard describes, because it includes generic, inline, and subunit
  dependencies.

  An object file must be recreated by recompiling the corresponding source
  file if any of the source files on which it depends are modified. For
  example, if the ``make`` utility is used to control compilation,
  the rule for an Ada object file must mention all the source files on
  which the object file depends, according to the above definition.
  The determination of the necessary
  recompilations is done automatically when one uses ``gnatmake``.

.. _The_Ada_Library_Information_Files:

The Ada Library Information Files
=================================

.. index:: Ada Library Information files

.. index:: ALI files

Each compilation actually generates two output files. The first of these
is the normal object file that has a :file:`.o` extension. The second is a
text file containing full dependency information. It has the same
name as the source file, but an :file:`.ali` extension.
This file is known as the Ada Library Information (:file:`ALI`) file.
The following information is contained in the :file:`ALI` file.

* Version information (indicates which version of GNAT was used to compile
  the unit(s) in question)

* Main program information (including priority and time slice settings,
  as well as the wide character encoding used during compilation).

* List of arguments used in the ``gcc`` command for the compilation

* Attributes of the unit, including configuration pragmas used, an indication
  of whether the compilation was successful, exception model used etc.

* A list of relevant restrictions applying to the unit (used for consistency)
  checking.

* Categorization information (e.g., use of pragma ``Pure``).

* Information on all |withed| units, including presence of
  ``Elaborate`` or ``Elaborate_All`` pragmas.

* Information from any ``Linker_Options`` pragmas used in the unit

* Information on the use of ``Body_Version`` or ``Version``
  attributes in the unit.

* Dependency information. This is a list of files, together with
  time stamp and checksum information. These are files on which
  the unit depends in the sense that recompilation is required
  if any of these units are modified.

* Cross-reference data. Contains information on all entities referenced
  in the unit. Used by tools like ``gnatxref`` and ``gnatfind`` to
  provide cross-reference information.

For a full detailed description of the format of the :file:`ALI` file,
see the source of the body of unit ``Lib.Writ``, contained in file
:file:`lib-writ.adb` in the GNAT compiler sources.


.. _Binding_an_Ada_Program:

Binding an Ada Program
======================

When using languages such as C and C++, once the source files have been
compiled the only remaining step in building an executable program
is linking the object modules together. This means that it is possible to
link an inconsistent version of a program, in which two units have
included different versions of the same header.

The rules of Ada do not permit such an inconsistent program to be built.
For example, if two clients have different versions of the same package,
it is illegal to build a program containing these two clients.
These rules are enforced by the GNAT binder, which also determines an
elaboration order consistent with the Ada rules.

The GNAT binder is run after all the object files for a program have
been created. It is given the name of the main program unit, and from
this it determines the set of units required by the program, by reading the
corresponding ALI files. It generates error messages if the program is
inconsistent or if no valid order of elaboration exists.

If no errors are detected, the binder produces a main program, in Ada by
default, that contains calls to the elaboration procedures of those
compilation unit that require them, followed by
a call to the main program. This Ada program is compiled to generate the
object file for the main program. The name of
the Ada file is :file:`b~xxx`.adb` (with the corresponding spec
:file:`b~xxx`.ads`) where ``xxx`` is the name of the
main program unit.

Finally, the linker is used to build the resulting executable program,
using the object from the main program from the bind step as well as the
object files for the Ada units of the program.


.. _GNAT_and_Libraries:

GNAT and Libraries
==================

.. index:: Library building and using

This section describes how to build and use libraries with GNAT, and also shows
how to recompile the GNAT run-time library. You should be familiar with the
Project Manager facility (see the *GNAT_Project_Manager* chapter of the
*GPRbuild User's Guide*) before reading this chapter.

.. _Introduction_to_Libraries_in_GNAT:

Introduction to Libraries in GNAT
---------------------------------

A library is, conceptually, a collection of objects which does not have its
own main thread of execution, but rather provides certain services to the
applications that use it. A library can be either statically linked with the
application, in which case its code is directly included in the application,
or, on platforms that support it, be dynamically linked, in which case
its code is shared by all applications making use of this library.

GNAT supports both types of libraries.
In the static case, the compiled code can be provided in different ways. The
simplest approach is to provide directly the set of objects resulting from
compilation of the library source files. Alternatively, you can group the
objects into an archive using whatever commands are provided by the operating
system. For the latter case, the objects are grouped into a shared library.

In the GNAT environment, a library has three types of components:

*  Source files,

*  :file:`ALI` files (see :ref:`The_Ada_Library_Information_Files`), and

*  Object files, an archive or a shared library.

A GNAT library may expose all its source files, which is useful for
documentation purposes. Alternatively, it may expose only the units needed by
an external user to make use of the library. That is to say, the specs
reflecting the library services along with all the units needed to compile
those specs, which can include generic bodies or any body implementing an
inlined routine. In the case of *stand-alone libraries* those exposed
units are called *interface units* (:ref:`Stand-alone_Ada_Libraries`).

All compilation units comprising an application, including those in a library,
need to be elaborated in an order partially defined by Ada's semantics. GNAT
computes the elaboration order from the :file:`ALI` files and this is why they
constitute a mandatory part of GNAT libraries.
*Stand-alone libraries* are the exception to this rule because a specific
library elaboration routine is produced independently of the application(s)
using the library.

.. _General_Ada_Libraries:

General Ada Libraries
---------------------


.. _Building_a_library:

Building a library
^^^^^^^^^^^^^^^^^^

The easiest way to build a library is to use the Project Manager,
which supports a special type of project called a *Library Project*
(see the *Library Projects* section in the *GNAT Project Manager*
chapter of the *GPRbuild User's Guide*).

A project is considered a library project, when two project-level attributes
are defined in it: ``Library_Name`` and ``Library_Dir``. In order to
control different aspects of library configuration, additional optional
project-level attributes can be specified:

* ``Library_Kind``
    This attribute controls whether the library is to be static or dynamic


* ``Library_Version``
    This attribute specifies the library version; this value is used
    during dynamic linking of shared libraries to determine if the currently
    installed versions of the binaries are compatible.

* ``Library_Options``

* ``Library_GCC``
    These attributes specify additional low-level options to be used during
    library generation, and redefine the actual application used to generate
    library.

The GNAT Project Manager takes full care of the library maintenance task,
including recompilation of the source files for which objects do not exist
or are not up to date, assembly of the library archive, and installation of
the library (i.e., copying associated source, object and :file:`ALI` files
to the specified location).

Here is a simple library project file:

.. code-block:: gpr

       project My_Lib is
         for Source_Dirs use ("src1", "src2");
         for Object_Dir use "obj";
         for Library_Name use "mylib";
         for Library_Dir use "lib";
         for Library_Kind use "dynamic";
       end My_lib;

and the compilation command to build and install the library:

.. code-block:: sh

     $ gnatmake -Pmy_lib

It is not entirely trivial to perform manually all the steps required to
produce a library. We recommend that you use the GNAT Project Manager
for this task. In special cases where this is not desired, the necessary
steps are discussed below.

There are various possibilities for compiling the units that make up the
library: for example with a Makefile (:ref:`Using_the_GNU_make_Utility`) or
with a conventional script. For simple libraries, it is also possible to create
a dummy main program which depends upon all the packages that comprise the
interface of the library. This dummy main program can then be given to
``gnatmake``, which will ensure that all necessary objects are built.

After this task is accomplished, you should follow the standard procedure
of the underlying operating system to produce the static or shared library.

Here is an example of such a dummy program:

.. code-block:: ada

       with My_Lib.Service1;
       with My_Lib.Service2;
       with My_Lib.Service3;
       procedure My_Lib_Dummy is
       begin
          null;
       end;

Here are the generic commands that will build an archive or a shared library.

.. code-block:: sh

     # compiling the library
     $ gnatmake -c my_lib_dummy.adb

     # we don't need the dummy object itself
     $ rm my_lib_dummy.o my_lib_dummy.ali

     # create an archive with the remaining objects
     $ ar rc libmy_lib.a *.o
     # some systems may require "ranlib" to be run as well

     # or create a shared library
     $ gcc -shared -o libmy_lib.so *.o
     # some systems may require the code to have been compiled with -fPIC

     # remove the object files that are now in the library
     $ rm *.o

     # Make the ALI files read-only so that gnatmake will not try to
     # regenerate the objects that are in the library
     $ chmod -w *.ali

Please note that the library must have a name of the form :file:`lib{xxx}.a`
or :file:`lib{xxx}.so` (or :file:`lib{xxx}.dll` on Windows) in order to
be accessed by the directive :switch:`-l{xxx}` at link time.

.. _Installing_a_library:

Installing a library
^^^^^^^^^^^^^^^^^^^^

.. index:: ADA_PROJECT_PATH
.. index:: GPR_PROJECT_PATH

If you use project files, library installation is part of the library build
process (see the *Installing a Library with Project Files* section of the
*GNAT Project Manager* chapter of the *GPRbuild User's Guide*).

When project files are not an option, it is also possible, but not recommended,
to install the library so that the sources needed to use the library are on the
Ada source path and the ALI files & libraries be on the Ada Object path (see
:ref:`Search_Paths_and_the_Run-Time_Library_RTL`. Alternatively, the system
administrator can place general-purpose libraries in the default compiler
paths, by specifying the libraries' location in the configuration files
:file:`ada_source_path` and :file:`ada_object_path`. These configuration files
must be located in the GNAT installation tree at the same place as the gcc spec
file. The location of the gcc spec file can be determined as follows:

.. code-block:: sh

     $ gcc -v


The configuration files mentioned above have a simple format: each line
must contain one unique directory name.
Those names are added to the corresponding path
in their order of appearance in the file. The names can be either absolute
or relative; in the latter case, they are relative to where theses files
are located.

The files :file:`ada_source_path` and :file:`ada_object_path` might not be
present in a
GNAT installation, in which case, GNAT will look for its run-time library in
the directories :file:`adainclude` (for the sources) and :file:`adalib` (for the
objects and :file:`ALI` files). When the files exist, the compiler does not
look in :file:`adainclude` and :file:`adalib`, and thus the
:file:`ada_source_path` file
must contain the location for the GNAT run-time sources (which can simply
be :file:`adainclude`). In the same way, the :file:`ada_object_path` file must
contain the location for the GNAT run-time objects (which can simply
be :file:`adalib`).

You can also specify a new default path to the run-time library at compilation
time with the switch :switch:`--RTS=rts-path`. You can thus choose / change
the run-time library you want your program to be compiled with. This switch is
recognized by ``gcc``, ``gnatmake``, ``gnatbind``,
``gnatls``, ``gnatfind`` and ``gnatxref``.

It is possible to install a library before or after the standard GNAT
library, by reordering the lines in the configuration files. In general, a
library must be installed before the GNAT library if it redefines
any part of it.

.. _Using_a_library:

Using a library
^^^^^^^^^^^^^^^

Once again, the project facility greatly simplifies the use of
libraries. In this context, using a library is just a matter of adding a
|with| clause in the user project. For instance, to make use of the
library ``My_Lib`` shown in examples in earlier sections, you can
write:

.. code-block:: gpr

       with "my_lib";
       project My_Proj is
         ...
       end My_Proj;

Even if you have a third-party, non-Ada library, you can still use GNAT's
Project Manager facility to provide a wrapper for it. For example, the
following project, when |withed| by your main project, will link with the
third-party library :file:`liba.a`:

.. code-block:: gpr

       project Liba is
          for Externally_Built use "true";
          for Source_Files use ();
          for Library_Dir use "lib";
          for Library_Name use "a";
          for Library_Kind use "static";
       end Liba;

This is an alternative to the use of ``pragma Linker_Options``. It is
especially interesting in the context of systems with several interdependent
static libraries where finding a proper linker order is not easy and best be
left to the tools having visibility over project dependence information.

In order to use an Ada library manually, you need to make sure that this
library is on both your source and object path
(see :ref:`Search_Paths_and_the_Run-Time_Library_RTL`
and :ref:`Search_Paths_for_gnatbind`). Furthermore, when the objects are grouped
in an archive or a shared library, you need to specify the desired
library at link time.

For example, you can use the library :file:`mylib` installed in
:file:`/dir/my_lib_src` and :file:`/dir/my_lib_obj` with the following commands:

.. code-block:: sh

     $ gnatmake -aI/dir/my_lib_src -aO/dir/my_lib_obj my_appl \\
       -largs -lmy_lib

This can be expressed more simply:

.. code-block:: sh

    $ gnatmake my_appl

when the following conditions are met:

* :file:`/dir/my_lib_src` has been added by the user to the environment
  variable :envvar:`ADA_INCLUDE_PATH`, or by the administrator to the file
  :file:`ada_source_path`

* :file:`/dir/my_lib_obj` has been added by the user to the environment
  variable :envvar:`ADA_OBJECTS_PATH`, or by the administrator to the file
  :file:`ada_object_path`

* a pragma ``Linker_Options`` has been added to one of the sources.
  For example:

  .. code-block:: ada

       pragma Linker_Options ("-lmy_lib");

Note that you may also load a library dynamically at
run time given its filename, as illustrated in the GNAT :file:`plugins` example
in the directory :file:`share/examples/gnat/plugins` within the GNAT
install area.

.. _Stand-alone_Ada_Libraries:

Stand-alone Ada Libraries
-------------------------

.. index:: ! Stand-alone libraries

.. _Introduction_to_Stand-alone_Libraries:

Introduction to Stand-alone Libraries
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A Stand-alone Library (abbreviated 'SAL') is a library that contains the
necessary code to
elaborate the Ada units that are included in the library. In contrast with
an ordinary library, which consists of all sources, objects and :file:`ALI`
files of the
library, a SAL may specify a restricted subset of compilation units
to serve as a library interface. In this case, the fully
self-sufficient set of files will normally consist of an objects
archive, the sources of interface units' specs, and the :file:`ALI`
files of interface units.
If an interface spec contains a generic unit or an inlined subprogram,
the body's
source must also be provided; if the units that must be provided in the source
form depend on other units, the source and :file:`ALI` files of those must
also be provided.

The main purpose of a SAL is to minimize the recompilation overhead of client
applications when a new version of the library is installed. Specifically,
if the interface sources have not changed, client applications do not need to
be recompiled. If, furthermore, a SAL is provided in the shared form and its
version, controlled by ``Library_Version`` attribute, is not changed,
then the clients do not need to be relinked.

SALs also allow the library providers to minimize the amount of library source
text exposed to the clients.  Such 'information hiding' might be useful or
necessary for various reasons.

Stand-alone libraries are also well suited to be used in an executable whose
main routine is not written in Ada.

.. _Building_a_Stand-alone_Library:

Building a Stand-alone Library
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

GNAT's Project facility provides a simple way of building and installing
stand-alone libraries; see the *Stand-alone Library Projects* section
in the *GNAT Project Manager* chapter of the *GPRbuild User's Guide*.
To be a Stand-alone Library Project, in addition to the two attributes
that make a project a Library Project (``Library_Name`` and
``Library_Dir``; see the *Library Projects* section in the
*GNAT Project Manager* chapter of the *GPRbuild User's Guide*),
the attribute ``Library_Interface`` must be defined.  For example:

.. code-block:: gpr

       for Library_Dir use "lib_dir";
       for Library_Name use "dummy";
       for Library_Interface use ("int1", "int1.child");

Attribute ``Library_Interface`` has a non-empty string list value,
each string in the list designating a unit contained in an immediate source
of the project file.

When a Stand-alone Library is built, first the binder is invoked to build
a package whose name depends on the library name
(:file:`b~dummy.ads/b` in the example above).
This binder-generated package includes initialization and
finalization procedures whose
names depend on the library name (``dummyinit`` and ``dummyfinal``
in the example
above). The object corresponding to this package is included in the library.

You must ensure timely (e.g., prior to any use of interfaces in the SAL)
calling of these procedures if a static SAL is built, or if a shared SAL
is built
with the project-level attribute ``Library_Auto_Init`` set to
``"false"``.

For a Stand-Alone Library, only the :file:`ALI` files of the Interface Units
(those that are listed in attribute ``Library_Interface``) are copied to
the Library Directory. As a consequence, only the Interface Units may be
imported from Ada units outside of the library. If other units are imported,
the binding phase will fail.

It is also possible to build an encapsulated library where not only
the code to elaborate and finalize the library is embedded but also
ensuring that the library is linked only against static
libraries. So an encapsulated library only depends on system
libraries, all other code, including the GNAT runtime, is embedded. To
build an encapsulated library the attribute
``Library_Standalone`` must be set to ``encapsulated``:

.. code-block:: gpr

       for Library_Dir use "lib_dir";
       for Library_Name use "dummy";
       for Library_Kind use "dynamic";
       for Library_Interface use ("int1", "int1.child");
       for Library_Standalone use "encapsulated";

The default value for this attribute is ``standard`` in which case
a stand-alone library is built.

The attribute ``Library_Src_Dir`` may be specified for a
Stand-Alone Library. ``Library_Src_Dir`` is a simple attribute that has a
single string value. Its value must be the path (absolute or relative to the
project directory) of an existing directory. This directory cannot be the
object directory or one of the source directories, but it can be the same as
the library directory. The sources of the Interface
Units of the library that are needed by an Ada client of the library will be
copied to the designated directory, called the Interface Copy directory.
These sources include the specs of the Interface Units, but they may also
include bodies and subunits, when pragmas ``Inline`` or ``Inline_Always``
are used, or when there is a generic unit in the spec. Before the sources
are copied to the Interface Copy directory, an attempt is made to delete all
files in the Interface Copy directory.

Building stand-alone libraries by hand is somewhat tedious, but for those
occasions when it is necessary here are the steps that you need to perform:

* Compile all library sources.

* Invoke the binder with the switch :switch:`-n` (No Ada main program),
  with all the :file:`ALI` files of the interfaces, and
  with the switch :switch:`-L` to give specific names to the ``init``
  and ``final`` procedures.  For example:

  .. code-block:: sh

      $ gnatbind -n int1.ali int2.ali -Lsal1

* Compile the binder generated file:

  .. code-block:: sh

      $ gcc -c b~int2.adb

* Link the dynamic library with all the necessary object files,
  indicating to the linker the names of the ``init`` (and possibly
  ``final``) procedures for automatic initialization (and finalization).
  The built library should be placed in a directory different from
  the object directory.

* Copy the ``ALI`` files of the interface to the library directory,
  add in this copy an indication that it is an interface to a SAL
  (i.e., add a word ``SL`` on the line in the :file:`ALI` file that starts
  with letter 'P') and make the modified copy of the :file:`ALI` file
  read-only.

Using SALs is not different from using other libraries
(see :ref:`Using_a_library`).

.. _Creating_a_Stand-alone_Library_to_be_used_in_a_non-Ada_context:

Creating a Stand-alone Library to be used in a non-Ada context
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

It is easy to adapt the SAL build procedure discussed above for use of a SAL in
a non-Ada context.

The only extra step required is to ensure that library interface subprograms
are compatible with the main program, by means of ``pragma Export``
or ``pragma Convention``.

Here is an example of simple library interface for use with C main program:

.. code-block:: ada

       package My_Package is

          procedure Do_Something;
          pragma Export (C, Do_Something, "do_something");

          procedure Do_Something_Else;
          pragma Export (C, Do_Something_Else, "do_something_else");

       end My_Package;

On the foreign language side, you must provide a 'foreign' view of the
library interface; remember that it should contain elaboration routines in
addition to interface subprograms.

The example below shows the content of :file:`mylib_interface.h` (note
that there is no rule for the naming of this file, any name can be used)

.. code-block:: c

       /* the library elaboration procedure */
       extern void mylibinit (void);

       /* the library finalization procedure */
       extern void mylibfinal (void);

       /* the interface exported by the library */
       extern void do_something (void);
       extern void do_something_else (void);

Libraries built as explained above can be used from any program, provided
that the elaboration procedures (named ``mylibinit`` in the previous
example) are called before the library services are used. Any number of
libraries can be used simultaneously, as long as the elaboration
procedure of each library is called.

Below is an example of a C program that uses the ``mylib`` library.

.. code-block:: c

       #include "mylib_interface.h"

       int
       main (void)
       {
          /* First, elaborate the library before using it */
          mylibinit ();

          /* Main program, using the library exported entities */
          do_something ();
          do_something_else ();

          /* Library finalization at the end of the program */
          mylibfinal ();
          return 0;
       }

Note that invoking any library finalization procedure generated by
``gnatbind`` shuts down the Ada run-time environment.
Consequently, the
finalization of all Ada libraries must be performed at the end of the program.
No call to these libraries or to the Ada run-time library should be made
after the finalization phase.

Note also that special care must be taken with multi-tasks
applications. The initialization and finalization routines are not
protected against concurrent access. If such requirement is needed it
must be ensured at the application level using a specific operating
system services like a mutex or a critical-section.

.. _Restrictions_in_Stand-alone_Libraries:

Restrictions in Stand-alone Libraries
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The pragmas listed below should be used with caution inside libraries,
as they can create incompatibilities with other Ada libraries:

* pragma ``Locking_Policy``
* pragma ``Partition_Elaboration_Policy``
* pragma ``Queuing_Policy``
* pragma ``Task_Dispatching_Policy``
* pragma ``Unreserve_All_Interrupts``

When using a library that contains such pragmas, the user must make sure
that all libraries use the same pragmas with the same values. Otherwise,
``Program_Error`` will
be raised during the elaboration of the conflicting
libraries. The usage of these pragmas and its consequences for the user
should therefore be well documented.

Similarly, the traceback in the exception occurrence mechanism should be
enabled or disabled in a consistent manner across all libraries.
Otherwise, Program_Error will be raised during the elaboration of the
conflicting libraries.

If the ``Version`` or ``Body_Version``
attributes are used inside a library, then you need to
perform a ``gnatbind`` step that specifies all :file:`ALI` files in all
libraries, so that version identifiers can be properly computed.
In practice these attributes are rarely used, so this is unlikely
to be a consideration.

.. _Rebuilding_the_GNAT_Run-Time_Library:

Rebuilding the GNAT Run-Time Library
------------------------------------

.. index:: GNAT Run-Time Library, rebuilding
.. index:: Building the GNAT Run-Time Library
.. index:: Rebuilding the GNAT Run-Time Library
.. index:: Run-Time Library, rebuilding

It may be useful to recompile the GNAT library in various contexts, the
most important one being the use of partition-wide configuration pragmas
such as ``Normalize_Scalars``. A special Makefile called
:file:`Makefile.adalib` is provided to that effect and can be found in
the directory containing the GNAT library. The location of this
directory depends on the way the GNAT environment has been installed and can
be determined by means of the command:

.. code-block:: sh

      $ gnatls -v

The last entry in the object search path usually contains the
gnat library. This Makefile contains its own documentation and in
particular the set of instructions needed to rebuild a new library and
to use it.


.. index:: ! Conditional compilation

.. _Conditional_Compilation:

Conditional Compilation
=======================

This section presents some guidelines for modeling conditional compilation in Ada and describes the
gnatprep preprocessor utility.

.. index:: ! Conditional compilation

.. _Modeling_Conditional_Compilation_in_Ada:

Modeling Conditional Compilation in Ada
---------------------------------------

It is often necessary to arrange for a single source program
to serve multiple purposes, where it is compiled in different
ways to achieve these different goals. Some examples of the
need for this feature are

* Adapting a program to a different hardware environment
* Adapting a program to a different target architecture
* Turning debugging features on and off
* Arranging for a program to compile with different compilers

In C, or C++, the typical approach would be to use the preprocessor
that is defined as part of the language. The Ada language does not
contain such a feature. This is not an oversight, but rather a very
deliberate design decision, based on the experience that overuse of
the preprocessing features in C and C++ can result in programs that
are extremely difficult to maintain. For example, if we have ten
switches that can be on or off, this means that there are a thousand
separate programs, any one of which might not even be syntactically
correct, and even if syntactically correct, the resulting program
might not work correctly. Testing all combinations can quickly become
impossible.

Nevertheless, the need to tailor programs certainly exists, and in
this section we will discuss how this can
be achieved using Ada in general, and GNAT in particular.

.. _Use_of_Boolean_Constants:

Use of Boolean Constants
^^^^^^^^^^^^^^^^^^^^^^^^

In the case where the difference is simply which code
sequence is executed, the cleanest solution is to use Boolean
constants to control which code is executed.

.. code-block:: ada

      FP_Initialize_Required : constant Boolean := True;
      ...
      if FP_Initialize_Required then
      ...
      end if;

Not only will the code inside the ``if`` statement not be executed if
the constant Boolean is ``False``, but it will also be completely
deleted from the program.
However, the code is only deleted after the ``if`` statement
has been checked for syntactic and semantic correctness.
(In contrast, with preprocessors the code is deleted before the
compiler ever gets to see it, so it is not checked until the switch
is turned on.)

.. index:: Preprocessors (contrasted with conditional compilation)

Typically the Boolean constants will be in a separate package,
something like:

.. code-block:: ada

       package Config is
          FP_Initialize_Required : constant Boolean := True;
          Reset_Available        : constant Boolean := False;
          ...
       end Config;

The ``Config`` package exists in multiple forms for the various targets,
with an appropriate script selecting the version of ``Config`` needed.
Then any other unit requiring conditional compilation can do a |with|
of ``Config`` to make the constants visible.

.. _Debugging_-_A_Special_Case:

Debugging - A Special Case
^^^^^^^^^^^^^^^^^^^^^^^^^^

A common use of conditional code is to execute statements (for example
dynamic checks, or output of intermediate results) under control of a
debug switch, so that the debugging behavior can be turned on and off.
This can be done using a Boolean constant to control whether the code
is active:

.. code-block:: ada

       if Debugging then
          Put_Line ("got to the first stage!");
       end if;

or

.. code-block:: ada

       if Debugging and then Temperature > 999.0 then
          raise Temperature_Crazy;
       end if;

.. index:: pragma Assert

Since this is a common case, there are special features to deal with
this in a convenient manner. For the case of tests, Ada 2005 has added
a pragma ``Assert`` that can be used for such tests. This pragma is modeled
on the ``Assert`` pragma that has always been available in GNAT, so this
feature may be used with GNAT even if you are not using Ada 2005 features.
The use of pragma ``Assert`` is described in the
:title:`GNAT_Reference_Manual`, but as an
example, the last test could be written:

.. code-block:: ada

       pragma Assert (Temperature <= 999.0, "Temperature Crazy");

or simply

.. code-block:: ada

       pragma Assert (Temperature <= 999.0);

In both cases, if assertions are active and the temperature is excessive,
the exception ``Assert_Failure`` will be raised, with the given string in
the first case or a string indicating the location of the pragma in the second
case used as the exception message.

.. index:: pragma Assertion_Policy

You can turn assertions on and off by using the ``Assertion_Policy``
pragma.

.. index:: -gnata switch

This is an Ada 2005 pragma which is implemented in all modes by
GNAT. Alternatively, you can use the :switch:`-gnata` switch
to enable assertions from the command line, which applies to
all versions of Ada.

.. index:: pragma Debug

For the example above with the ``Put_Line``, the GNAT-specific pragma
``Debug`` can be used:

.. code-block:: ada

       pragma Debug (Put_Line ("got to the first stage!"));

If debug pragmas are enabled, the argument, which must be of the form of
a procedure call, is executed (in this case, ``Put_Line`` will be called).
Only one call can be present, but of course a special debugging procedure
containing any code you like can be included in the program and then
called in a pragma ``Debug`` argument as needed.

One advantage of pragma ``Debug`` over the ``if Debugging then``
construct is that pragma ``Debug`` can appear in declarative contexts,
such as at the very beginning of a procedure, before local declarations have
been elaborated.

.. index:: pragma Debug_Policy

Debug pragmas are enabled using either the :switch:`-gnata` switch that also
controls assertions, or with a separate Debug_Policy pragma.

The latter pragma is new in the Ada 2005 versions of GNAT (but it can be used
in Ada 95 and Ada 83 programs as well), and is analogous to
pragma ``Assertion_Policy`` to control assertions.

``Assertion_Policy`` and ``Debug_Policy`` are configuration pragmas,
and thus they can appear in :file:`gnat.adc` if you are not using a
project file, or in the file designated to contain configuration pragmas
in a project file.
They then apply to all subsequent compilations. In practice the use of
the :switch:`-gnata` switch is often the most convenient method of controlling
the status of these pragmas.

Note that a pragma is not a statement, so in contexts where a statement
sequence is required, you can't just write a pragma on its own. You have
to add a ``null`` statement.

.. code-block:: ada

       if ... then
          ... -- some statements
       else
          pragma Assert (Num_Cases < 10);
          null;
       end if;

.. _Conditionalizing_Declarations:

Conditionalizing Declarations
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In some cases it may be necessary to conditionalize declarations to meet
different requirements. For example we might want a bit string whose length
is set to meet some hardware message requirement.

This may be possible using declare blocks controlled
by conditional constants:

.. code-block:: ada

       if Small_Machine then
          declare
             X : Bit_String (1 .. 10);
          begin
             ...
          end;
       else
          declare
             X : Large_Bit_String (1 .. 1000);
          begin
             ...
          end;
       end if;

Note that in this approach, both declarations are analyzed by the
compiler so this can only be used where both declarations are legal,
even though one of them will not be used.

Another approach is to define integer constants, e.g., ``Bits_Per_Word``,
or Boolean constants, e.g., ``Little_Endian``, and then write declarations
that are parameterized by these constants. For example

.. code-block:: ada

       for Rec use
         Field1 at 0 range Boolean'Pos (Little_Endian) * 10 .. Bits_Per_Word;
       end record;

If ``Bits_Per_Word`` is set to 32, this generates either

.. code-block:: ada

       for Rec use
         Field1 at 0 range 0 .. 32;
       end record;

for the big endian case, or

.. code-block:: ada

       for Rec use record
           Field1 at 0 range 10 .. 32;
       end record;

for the little endian case. Since a powerful subset of Ada expression
notation is usable for creating static constants, clever use of this
feature can often solve quite difficult problems in conditionalizing
compilation (note incidentally that in Ada 95, the little endian
constant was introduced as ``System.Default_Bit_Order``, so you do not
need to define this one yourself).

.. _Use_of_Alternative_Implementations:

Use of Alternative Implementations
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In some cases, none of the approaches described above are adequate. This
can occur for example if the set of declarations required is radically
different for two different configurations.

In this situation, the official Ada way of dealing with conditionalizing
such code is to write separate units for the different cases. As long as
this does not result in excessive duplication of code, this can be done
without creating maintenance problems. The approach is to share common
code as far as possible, and then isolate the code and declarations
that are different. Subunits are often a convenient method for breaking
out a piece of a unit that is to be conditionalized, with separate files
for different versions of the subunit for different targets, where the
build script selects the right one to give to the compiler.

.. index:: Subunits (and conditional compilation)

As an example, consider a situation where a new feature in Ada 2005
allows something to be done in a really nice way. But your code must be able
to compile with an Ada 95 compiler. Conceptually you want to say:

.. code-block:: ada

       if Ada_2005 then
          ... neat Ada 2005 code
       else
          ... not quite as neat Ada 95 code
       end if;

where ``Ada_2005`` is a Boolean constant.

But this won't work when ``Ada_2005`` is set to ``False``,
since the ``then`` clause will be illegal for an Ada 95 compiler.
(Recall that although such unreachable code would eventually be deleted
by the compiler, it still needs to be legal.  If it uses features
introduced in Ada 2005, it will be illegal in Ada 95.)

So instead we write

.. code-block:: ada

       procedure Insert is separate;

Then we have two files for the subunit ``Insert``, with the two sets of
code.
If the package containing this is called ``File_Queries``, then we might
have two files

* :file:`file_queries-insert-2005.adb`
* :file:`file_queries-insert-95.adb`

and the build script renames the appropriate file to :file:`file_queries-insert.adb` and then carries out the compilation.

This can also be done with project files' naming schemes. For example:

.. code-block:: gpr

       for body ("File_Queries.Insert") use "file_queries-insert-2005.ada";

Note also that with project files it is desirable to use a different extension
than :file:`ads` / :file:`adb` for alternative versions. Otherwise a naming
conflict may arise through another commonly used feature: to declare as part
of the project a set of directories containing all the sources obeying the
default naming scheme.

The use of alternative units is certainly feasible in all situations,
and for example the Ada part of the GNAT run-time is conditionalized
based on the target architecture using this approach. As a specific example,
consider the implementation of the AST feature in VMS. There is one
spec: :file:`s-asthan.ads` which is the same for all architectures, and three
bodies:

* :file:`s-asthan.adb`
    used for all non-VMS operating systems

* :file:`s-asthan-vms-alpha.adb`
    used for VMS on the Alpha

* :file:`s-asthan-vms-ia64.adb`
    used for VMS on the ia64

The dummy version :file:`s-asthan.adb` simply raises exceptions noting that
this operating system feature is not available, and the two remaining
versions interface with the corresponding versions of VMS to provide
VMS-compatible AST handling. The GNAT build script knows the architecture
and operating system, and automatically selects the right version,
renaming it if necessary to :file:`s-asthan.adb` before the run-time build.

Another style for arranging alternative implementations is through Ada's
access-to-subprogram facility.
In case some functionality is to be conditionally included,
you can declare an access-to-procedure variable ``Ref`` that is initialized
to designate a 'do nothing' procedure, and then invoke ``Ref.all``
when appropriate.
In some library package, set ``Ref`` to ``Proc'Access`` for some
procedure ``Proc`` that performs the relevant processing.
The initialization only occurs if the library package is included in the
program.
The same idea can also be implemented using tagged types and dispatching
calls.

.. _Preprocessing:

Preprocessing
^^^^^^^^^^^^^

.. index:: Preprocessing

Although it is quite possible to conditionalize code without the use of
C-style preprocessing, as described earlier in this section, it is
nevertheless convenient in some cases to use the C approach. Moreover,
older Ada compilers have often provided some preprocessing capability,
so legacy code may depend on this approach, even though it is not
standard.

To accommodate such use, GNAT provides a preprocessor (modeled to a large
extent on the various preprocessors that have been used
with legacy code on other compilers, to enable easier transition).

.. index:: gnatprep

The preprocessor may be used in two separate modes. It can be used quite
separately from the compiler, to generate a separate output source file
that is then fed to the compiler as a separate step. This is the
``gnatprep`` utility, whose use is fully described in
:ref:`Preprocessing_with_gnatprep`.

The preprocessing language allows such constructs as

.. code-block:: c

       #if DEBUG or else (PRIORITY > 4) then
          sequence of declarations
       #else
          completely different sequence of declarations
       #end if;

The values of the symbols ``DEBUG`` and ``PRIORITY`` can be
defined either on the command line or in a separate file.

The other way of running the preprocessor is even closer to the C style and
often more convenient. In this approach the preprocessing is integrated into
the compilation process. The compiler is given the preprocessor input which
includes ``#if`` lines etc, and then the compiler carries out the
preprocessing internally and processes the resulting output.
For more details on this approach, see :ref:`Integrated_Preprocessing`.

.. _Preprocessing_with_gnatprep:

Preprocessing with ``gnatprep``
-------------------------------

.. index:: ! gnatprep
.. index:: Preprocessing (gnatprep)

This section discusses how to use GNAT's ``gnatprep`` utility for simple
preprocessing.
Although designed for use with GNAT, ``gnatprep`` does not depend on any
special GNAT features.
For further discussion of conditional compilation in general, see
:ref:`Conditional_Compilation`.

.. _Preprocessing_Symbols:

Preprocessing Symbols
^^^^^^^^^^^^^^^^^^^^^

Preprocessing symbols are defined in *definition files* and referenced in the
sources to be preprocessed. A preprocessing symbol is an identifier, following
normal Ada (case-insensitive) rules for its syntax, with the restriction that
all characters need to be in the ASCII set (no accented letters).

.. _Using_gnatprep:

Using ``gnatprep``
^^^^^^^^^^^^^^^^^^

To call ``gnatprep`` use:

.. code-block:: sh

    $ gnatprep [ switches ] infile outfile [ deffile ]

where

* *switches*
    is an optional sequence of switches as described in the next section.

* *infile*
    is the full name of the input file, which is an Ada source
    file containing preprocessor directives.

* *outfile*
    is the full name of the output file, which is an Ada source
    in standard Ada form. When used with GNAT, this file name will
    normally have an ``ads`` or ``adb`` suffix.

* ``deffile``
    is the full name of a text file containing definitions of
    preprocessing symbols to be referenced by the preprocessor. This argument is
    optional, and can be replaced by the use of the :switch:`-D` switch.


.. _Switches_for_gnatprep:

Switches for ``gnatprep``
^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: --version (gnatprep)

:switch:`--version`
  Display Copyright and version, then exit disregarding all other options.

.. index:: --help (gnatprep)

:switch:`--help`
  If :switch:`--version` was not used, display usage and then exit disregarding
  all other options.

.. index:: -b (gnatprep)

:switch:`-b`
  Causes both preprocessor lines and the lines deleted by
  preprocessing to be replaced by blank lines in the output source file,
  preserving line numbers in the output file.

.. index:: -c (gnatprep)

:switch:`-c`
  Causes both preprocessor lines and the lines deleted
  by preprocessing to be retained in the output source as comments marked
  with the special string ``"--! "``. This option will result in line numbers
  being preserved in the output file.

.. index:: -C (gnatprep)

:switch:`-C`
  Causes comments to be scanned. Normally comments are ignored by gnatprep.
  If this option is specified, then comments are scanned and any $symbol
  substitutions performed as in program text. This is particularly useful
  when structured comments are used (e.g., for programs written in a
  pre-2014 version of the SPARK Ada subset). Note that this switch is not
  available when  doing integrated preprocessing (it would be useless in
  this context since comments are ignored by the compiler in any case).

.. index:: -D (gnatprep)

:switch:`-D{symbol}[={value}]`
  Defines a new preprocessing symbol with the specified value. If no value is given
  on the command line, then symbol is considered to be ``True``. This switch
  can be used in place of a definition file.

.. index:: -r (gnatprep)

:switch:`-r`
  Causes a ``Source_Reference`` pragma to be generated that
  references the original input file, so that error messages will use
  the file name of this original file. The use of this switch implies
  that preprocessor lines are not to be removed from the file, so its
  use will force ``-b`` mode if ``-c``
  has not been specified explicitly.

  Note that if the file to be preprocessed contains multiple units, then
  it will be necessary to ``gnatchop`` the output file from
  ``gnatprep``. If a ``Source_Reference`` pragma is present
  in the preprocessed file, it will be respected by
  ``gnatchop -r``
  so that the final chopped files will correctly refer to the original
  input source file for ``gnatprep``.

.. index:: -s (gnatprep)

:switch:`-s`
  Causes a sorted list of symbol names and values to be
  listed on the standard output file.

.. index:: -T (gnatprep)

:switch:`-T`
  Use LF as line terminators when writing files. By default the line terminator
  of the host (LF under unix, CR/LF under Windows) is used.

.. index:: -u (gnatprep)

:switch:`-u`
  Causes undefined symbols to be treated as having the value FALSE in the context
  of a preprocessor test. In the absence of this option, an undefined symbol in
  a ``#if`` or ``#elsif`` test will be treated as an error.

.. index:: -v (gnatprep)

:switch:`-v`
  Verbose mode: generates more output about work done.


Note: if neither :switch:`-b` nor :switch:`-c` is present,
then preprocessor lines and
deleted lines are completely removed from the output, unless -r is
specified, in which case -b is assumed.


.. _Form_of_Definitions_File:

Form of Definitions File
^^^^^^^^^^^^^^^^^^^^^^^^

The definitions file contains lines of the form::

      symbol := value

where ``symbol`` is a preprocessing symbol, and ``value`` is one of the following:

*  Empty, corresponding to a null substitution,
*  A string literal using normal Ada syntax, or
*  Any sequence of characters from the set {letters, digits, period, underline}.

Comment lines may also appear in the definitions file, starting with
the usual ``--``,
and comments may be added to the definitions lines.


.. _Form_of_Input_Text_for_gnatprep:

Form of Input Text for ``gnatprep``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The input text may contain preprocessor conditional inclusion lines,
as well as general symbol substitution sequences.

The preprocessor conditional inclusion commands have the form:

.. code-block:: c

       #if <expression> [then]
          lines
       #elsif <expression> [then]
          lines
       #elsif <expression> [then]
          lines
       ...
       #else
          lines
       #end if;

In this example, <expression> is defined by the following grammar::

       <expression> ::=  <symbol>
       <expression> ::=  <symbol> = "<value>"
       <expression> ::=  <symbol> = <symbol>
       <expression> ::=  <symbol> = <integer>
       <expression> ::=  <symbol> > <integer>
       <expression> ::=  <symbol> >= <integer>
       <expression> ::=  <symbol> < <integer>
       <expression> ::=  <symbol> <= <integer>
       <expression> ::=  <symbol> 'Defined
       <expression> ::=  not <expression>
       <expression> ::=  <expression> and <expression>
       <expression> ::=  <expression> or <expression>
       <expression> ::=  <expression> and then <expression>
       <expression> ::=  <expression> or else <expression>
       <expression> ::=  ( <expression> )

Note the following restriction: it is not allowed to have "and" or "or"
following "not" in the same expression without parentheses. For example, this
is not allowed:

.. code-block:: ada

       not X or Y

This can be expressed instead as one of the following forms:

.. code-block:: ada

     (not X) or Y
     not (X or Y)

For the first test (<expression> ::= <symbol>) the symbol must have
either the value true or false, that is to say the right-hand of the
symbol definition must be one of the (case-insensitive) literals
``True`` or ``False``. If the value is true, then the
corresponding lines are included, and if the value is false, they are
excluded.

When comparing a symbol to an integer, the integer is any non negative
literal integer as defined in the Ada Reference Manual, such as 3, 16#FF# or
2#11#. The symbol value must also be a non negative integer. Integer values
in the range 0 .. 2**31-1 are supported.

The test (<expression> ::= <symbol>'Defined) is true only if
the symbol has been defined in the definition file or by a :switch:`-D`
switch on the command line. Otherwise, the test is false.

The equality tests are case insensitive, as are all the preprocessor lines.

If the symbol referenced is not defined in the symbol definitions file,
then the effect depends on whether or not switch :switch:`-u`
is specified. If so, then the symbol is treated as if it had the value
false and the test fails. If this switch is not specified, then
it is an error to reference an undefined symbol. It is also an error to
reference a symbol that is defined with a value other than ``True``
or ``False``.

The use of the ``not`` operator inverts the sense of this logical test.
The ``not`` operator cannot be combined with the ``or`` or ``and``
operators, without parentheses. For example, "if not X or Y then" is not
allowed, but "if (not X) or Y then" and "if not (X or Y) then" are.

The ``then`` keyword is optional as shown

The ``#`` must be the first non-blank character on a line, but
otherwise the format is free form. Spaces or tabs may appear between
the ``#`` and the keyword. The keywords and the symbols are case
insensitive as in normal Ada code. Comments may be used on a
preprocessor line, but other than that, no other tokens may appear on a
preprocessor line. Any number of ``elsif`` clauses can be present,
including none at all. The ``else`` is optional, as in Ada.

The ``#`` marking the start of a preprocessor line must be the first
non-blank character on the line, i.e., it must be preceded only by
spaces or horizontal tabs.

Symbol substitution outside of preprocessor lines is obtained by using
the sequence::

      $symbol

anywhere within a source line, except in a comment or within a
string literal. The identifier
following the ``$`` must match one of the symbols defined in the symbol
definition file, and the result is to substitute the value of the
symbol in place of ``$symbol`` in the output file.

Note that although the substitution of strings within a string literal
is not possible, it is possible to have a symbol whose defined value is
a string literal. So instead of setting XYZ to ``hello`` and writing:

.. code-block:: ada

     Header : String := "$XYZ";

you should set XYZ to ``"hello"`` and write:

.. code-block:: ada

     Header : String := $XYZ;

and then the substitution will occur as desired.


.. _Integrated_Preprocessing:

Integrated Preprocessing
------------------------

As noted above, a file to be preprocessed consists of Ada source code
in which preprocessing lines have been inserted. However,
instead of using ``gnatprep`` to explicitly preprocess a file as a separate
step before compilation, you can carry out the preprocessing implicitly
as part of compilation. Such *integrated preprocessing*, which is the common
style with C, is performed when either or both of the following switches
are passed to the compiler:

   *   :switch:`-gnatep`, which specifies the *preprocessor data file*.
       This file dictates how the source files will be preprocessed (e.g., which
       symbol definition files apply to which sources).

   *   :switch:`-gnateD`, which defines values for preprocessing symbols.

Integrated preprocessing applies only to Ada source files, it is
not available for configuration pragma files.

With integrated preprocessing, the output from the preprocessor is not,
by default, written to any external file. Instead it is passed
internally to the compiler. To preserve the result of
preprocessing in a file, either run ``gnatprep``
in standalone mode or else supply the :switch:`-gnateG` switch
(described below) to the compiler.

When using project files:

   *    the builder switch :switch:`-x` should be used if any Ada source is
        compiled with :switch:`gnatep=`, so that the compiler finds the
        *preprocessor data file*.

   *    the preprocessing data file and the symbol definition files should be
        located in the source directories of the project.

Note that the ``gnatmake`` switch :switch:`-m` will almost
always trigger recompilation for sources that are preprocessed,
because ``gnatmake`` cannot compute the checksum of the source after
preprocessing.

The actual preprocessing function is described in detail in
:ref:`Preprocessing_with_gnatprep`. This section explains the switches
that relate to integrated preprocessing.

.. index:: -gnatep (gcc)

:switch:`-gnatep={preprocessor_data_file}`
  This switch specifies the file name (without directory
  information) of the preprocessor data file. Either place this file
  in one of the source directories, or, when using project
  files, reference the project file's directory via the
  ``project_name'Project_Dir`` project attribute; e.g:

     .. code-block:: gpr

         project Prj is
            package Compiler is
               for Switches ("Ada") use
                 ("-gnatep=" & Prj'Project_Dir & "prep.def");
            end Compiler;
         end Prj;

  A preprocessor data file is a text file that contains *preprocessor
  control lines*.  A preprocessor control line directs the preprocessing of
  either a particular source file, or, analogous to ``others`` in Ada,
  all sources not specified elsewhere in  the preprocessor data file.
  A preprocessor control line
  can optionally identify a *definition file* that assigns values to
  preprocessor symbols, as well as a list of switches that relate to
  preprocessing.
  Empty lines and comments (using Ada syntax) are also permitted, with no
  semantic effect.

  Here's an example of a preprocessor data file:

    .. code-block:: ada

        "toto.adb"  "prep.def" -u
        --  Preprocess toto.adb, using definition file prep.def
        --  Undefined symbols are treated as False

        * -c -DVERSION=V101
        --  Preprocess all other sources without using a definition file
        --  Suppressed lined are commented
        --  Symbol VERSION has the value V101

        "tata.adb" "prep2.def" -s
        --  Preprocess tata.adb, using definition file prep2.def
        --  List all symbols with their values

  A preprocessor control line has the following syntax:

    ::

        <preprocessor_control_line> ::=
           <preprocessor_input> [ <definition_file_name> ] { <switch> }

        <preprocessor_input> ::= <source_file_name> | '*'

        <definition_file_name> ::= <string_literal>

        <source_file_name> := <string_literal>

        <switch> := (See below for list)

  Thus  each preprocessor control line starts with either a literal string or
  the character '*':

  *  A literal string is the file name (without directory information) of the source
     file that will be input to the preprocessor.

  *  The character '*' is a wild-card indicator; the additional parameters on the line
     indicate the preprocessing for all the sources
     that are not specified explicitly on other lines (the order of the lines is not
     significant).

  It is an error to have two lines with the same file name or two
  lines starting with the character '*'.

  After the file name or '*', an optional literal string specifies the name of
  the definition file to be used for preprocessing
  (:ref:`Form_of_Definitions_File`). The definition files are found by the
  compiler in one of the source directories. In some cases, when compiling
  a source in a directory other than the current directory, if the definition
  file is in the current directory, it may be necessary to add the current
  directory as a source directory through the :switch:`-I` switch; otherwise
  the compiler would not find the definition file.

  Finally, switches similar to those of ``gnatprep`` may optionally appear:

  :switch:`-b`
    Causes both preprocessor lines and the lines deleted by
    preprocessing to be replaced by blank lines, preserving the line number.
    This switch is always implied; however, if specified after :switch:`-c`
    it cancels the effect of :switch:`-c`.


  :switch:`-c`
    Causes both preprocessor lines and the lines deleted
    by preprocessing to be retained as comments marked
    with the special string '`--!`'.


  :switch:`-D{symbol}={new_value}`
    Define or redefine ``symbol`` to have ``new_value`` as its value.
    The permitted form for ``symbol`` is either an Ada identifier, or any Ada reserved word
    aside from ``if``,
    ``else``, ``elsif``, ``end``, ``and``, ``or`` and ``then``.
    The permitted form for ``new_value`` is a literal string, an Ada identifier or any Ada reserved
    word. A symbol declared with this switch replaces a symbol with the
    same name defined in a definition file.


  :switch:`-s`
    Causes a sorted list of symbol names and values to be
    listed on the standard output file.


  :switch:`-u`
    Causes undefined symbols to be treated as having the value ``FALSE``
    in the context
    of a preprocessor test. In the absence of this option, an undefined symbol in
    a ``#if`` or ``#elsif`` test will be treated as an error.


.. index:: -gnateD (gcc)

:switch:`-gnateD{symbol}[={new_value}]`
  Define or redefine ``symbol`` to have ``new_value`` as its value. If no value
  is supplied, then the value of ``symbol`` is ``True``.
  The form of ``symbol`` is an identifier, following normal Ada (case-insensitive)
  rules for its syntax, and ``new_value`` is either an arbitrary string between double
  quotes or any sequence (including an empty sequence) of characters from the
  set (letters, digits, period, underline).
  Ada reserved words may be used as symbols, with the exceptions of ``if``,
  ``else``, ``elsif``, ``end``, ``and``, ``or`` and ``then``.

  Examples:

    ::

        -gnateDToto=Tata
        -gnateDFoo
        -gnateDFoo=\"Foo-Bar\"

  A symbol declared with this switch on the command line replaces a
  symbol with the same name either in a definition file or specified with a
  switch :switch:`-D` in the preprocessor data file.

  This switch is similar to switch :switch:`-D` of ``gnatprep``.


:switch:`-gnateG`
  When integrated preprocessing is performed on source file :file:`filename.extension`,
  create or overwrite :file:`filename.extension.prep` to contain
  the result of the preprocessing.
  For example if the source file is :file:`foo.adb` then
  the output file will be :file:`foo.adb.prep`.


.. _Mixed_Language_Programming:

Mixed Language Programming
==========================

.. index:: Mixed Language Programming

This section describes how to develop a mixed-language program,
with a focus on combining Ada with C or C++.

.. _Interfacing_to_C:

Interfacing to C
----------------

Interfacing Ada with a foreign language such as C involves using
compiler directives to import and/or export entity definitions in each
language -- using ``extern`` statements in C, for instance, and the
``Import``, ``Export``, and ``Convention`` pragmas in Ada.
A full treatment of these topics is provided in Appendix B, section 1
of the Ada Reference Manual.

There are two ways to build a program using GNAT that contains some Ada
sources and some foreign language sources, depending on whether or not
the main subprogram is written in Ada.  Here is a source example with
the main subprogram in Ada:

.. code-block:: c

    /* file1.c */
    #include <stdio.h>

    void print_num (int num)
    {
      printf ("num is %d.\\n", num);
      return;
    }

.. code-block:: c

    /* file2.c */

    /* num_from_Ada is declared in my_main.adb */
    extern int num_from_Ada;

    int get_num (void)
    {
      return num_from_Ada;
    }

.. code-block:: ada

    --  my_main.adb
    procedure My_Main is

       --  Declare then export an Integer entity called num_from_Ada
       My_Num : Integer := 10;
       pragma Export (C, My_Num, "num_from_Ada");

       --  Declare an Ada function spec for Get_Num, then use
       --  C function get_num for the implementation.
       function Get_Num return Integer;
       pragma Import (C, Get_Num, "get_num");

       --  Declare an Ada procedure spec for Print_Num, then use
       --  C function print_num for the implementation.
       procedure Print_Num (Num : Integer);
       pragma Import (C, Print_Num, "print_num");

    begin
       Print_Num (Get_Num);
    end My_Main;

To build this example:

* First compile the foreign language files to
  generate object files:

  .. code-block:: sh

      $ gcc -c file1.c
      $ gcc -c file2.c

* Then, compile the Ada units to produce a set of object files and ALI
  files:

  .. code-block:: sh

      $ gnatmake -c my_main.adb

* Run the Ada binder on the Ada main program:

  .. code-block:: sh

      $ gnatbind my_main.ali

* Link the Ada main program, the Ada objects and the other language
  objects:

  .. code-block:: sh

      $ gnatlink my_main.ali file1.o file2.o

The last three steps can be grouped in a single command:

.. code-block:: sh

   $ gnatmake my_main.adb -largs file1.o file2.o


.. index:: Binder output file

If the main program is in a language other than Ada, then you may have
more than one entry point into the Ada subsystem. You must use a special
binder option to generate callable routines that initialize and
finalize the Ada units (:ref:`Binding_with_Non-Ada_Main_Programs`).
Calls to the initialization and finalization routines must be inserted
in the main program, or some other appropriate point in the code. The
call to initialize the Ada units must occur before the first Ada
subprogram is called, and the call to finalize the Ada units must occur
after the last Ada subprogram returns. The binder will place the
initialization and finalization subprograms into the
:file:`b~xxx.adb` file where they can be accessed by your C
sources.  To illustrate, we have the following example:

.. code-block:: c

     /* main.c */
     extern void adainit (void);
     extern void adafinal (void);
     extern int add (int, int);
     extern int sub (int, int);

     int main (int argc, char *argv[])
     {
        int a = 21, b = 7;

        adainit();

        /* Should print "21 + 7 = 28" */
        printf ("%d + %d = %d\\n", a, b, add (a, b));

        /* Should print "21 - 7 = 14" */
        printf ("%d - %d = %d\\n", a, b, sub (a, b));

        adafinal();
     }

.. code-block:: ada

     --  unit1.ads
     package Unit1 is
        function Add (A, B : Integer) return Integer;
        pragma Export (C, Add, "add");
     end Unit1;

.. code-block:: ada

     --  unit1.adb
     package body Unit1 is
        function Add (A, B : Integer) return Integer is
        begin
           return A + B;
        end Add;
     end Unit1;

.. code-block:: ada

     --  unit2.ads
     package Unit2 is
        function Sub (A, B : Integer) return Integer;
        pragma Export (C, Sub, "sub");
     end Unit2;

.. code-block:: ada

     --  unit2.adb
     package body Unit2 is
        function Sub (A, B : Integer) return Integer is
        begin
           return A - B;
        end Sub;
     end Unit2;

The build procedure for this application is similar to the last
example's:

* First, compile the foreign language files to generate object files:

  .. code-block:: sh

      $ gcc -c main.c


* Next, compile the Ada units to produce a set of object files and ALI
  files:

  .. code-block:: sh

      $ gnatmake -c unit1.adb
      $ gnatmake -c unit2.adb

* Run the Ada binder on every generated ALI file.  Make sure to use the
  :switch:`-n` option to specify a foreign main program:

  .. code-block:: sh

      $ gnatbind -n unit1.ali unit2.ali

* Link the Ada main program, the Ada objects and the foreign language
  objects. You need only list the last ALI file here:

  .. code-block:: sh

      $ gnatlink unit2.ali main.o -o exec_file

  This procedure yields a binary executable called :file:`exec_file`.

Depending on the circumstances (for example when your non-Ada main object
does not provide symbol ``main``), you may also need to instruct the
GNAT linker not to include the standard startup objects by passing the
:switch:`-nostartfiles` switch to ``gnatlink``.

.. _Calling_Conventions:

Calling Conventions
-------------------

.. index:: Foreign Languages

.. index:: Calling Conventions

GNAT follows standard calling sequence conventions and will thus interface
to any other language that also follows these conventions. The following
Convention identifiers are recognized by GNAT:


.. index:: Interfacing to Ada

.. index:: Other Ada compilers

.. index:: Convention Ada

``Ada``
  This indicates that the standard Ada calling sequence will be
  used and all Ada data items may be passed without any limitations in the
  case where GNAT is used to generate both the caller and callee. It is also
  possible to mix GNAT generated code and code generated by another Ada
  compiler. In this case, the data types should be restricted to simple
  cases, including primitive types. Whether complex data types can be passed
  depends on the situation. Probably it is safe to pass simple arrays, such
  as arrays of integers or floats. Records may or may not work, depending
  on whether both compilers lay them out identically. Complex structures
  involving variant records, access parameters, tasks, or protected types,
  are unlikely to be able to be passed.

  Note that in the case of GNAT running
  on a platform that supports HP Ada 83, a higher degree of compatibility
  can be guaranteed, and in particular records are laid out in an identical
  manner in the two compilers. Note also that if output from two different
  compilers is mixed, the program is responsible for dealing with elaboration
  issues. Probably the safest approach is to write the main program in the
  version of Ada other than GNAT, so that it takes care of its own elaboration
  requirements, and then call the GNAT-generated adainit procedure to ensure
  elaboration of the GNAT components. Consult the documentation of the other
  Ada compiler for further details on elaboration.

  However, it is not possible to mix the tasking run time of GNAT and
  HP Ada 83, All the tasking operations must either be entirely within
  GNAT compiled sections of the program, or entirely within HP Ada 83
  compiled sections of the program.

.. index:: Interfacing to Assembly

.. index:: Convention Assembler


``Assembler``
  Specifies assembler as the convention. In practice this has the
  same effect as convention Ada (but is not equivalent in the sense of being
  considered the same convention).

.. index:: Convention Asm

.. index:: Asm

``Asm``
  Equivalent to Assembler.

  .. index:: Interfacing to COBOL

  .. index:: Convention COBOL

.. index:: COBOL

``COBOL``
  Data will be passed according to the conventions described
  in section B.4 of the Ada Reference Manual.

.. index:: C
.. index:: Interfacing to C

.. index:: Convention C


``C``
  Data will be passed according to the conventions described
  in section B.3 of the Ada Reference Manual.

  A note on interfacing to a C 'varargs' function:

    .. index:: C varargs function
    .. index:: Interfacing to C varargs function
    .. index:: varargs function interfaces

    In C, ``varargs`` allows a function to take a variable number of
    arguments. There is no direct equivalent in this to Ada. One
    approach that can be used is to create a C wrapper for each
    different profile and then interface to this C wrapper. For
    example, to print an ``int`` value using ``printf``,
    create a C function ``printfi`` that takes two arguments, a
    pointer to a string and an int, and calls ``printf``.
    Then in the Ada program, use pragma ``Import`` to
    interface to ``printfi``.

    It may work on some platforms to directly interface to
    a ``varargs`` function by providing a specific Ada profile
    for a particular call. However, this does not work on
    all platforms, since there is no guarantee that the
    calling sequence for a two argument normal C function
    is the same as for calling a ``varargs`` C function with
    the same two arguments.

.. index:: Convention Default

.. index:: Default

``Default``
  Equivalent to C.

.. index:: Convention External

.. index:: External

``External``
  Equivalent to C.

.. index:: C++
.. index:: Interfacing to C++

.. index:: Convention C++


``C_Plus_Plus`` (or ``CPP``)
  This stands for C++. For most purposes this is identical to C.
  See the separate description of the specialized GNAT pragmas relating to
  C++ interfacing for further details.

.. index:: Fortran
.. index:: Interfacing to Fortran
.. index:: Convention Fortran


``Fortran``
  Data will be passed according to the conventions described
  in section B.5 of the Ada Reference Manual.


``Intrinsic``
  This applies to an intrinsic operation, as defined in the Ada
  Reference Manual. If a pragma Import (Intrinsic) applies to a subprogram,
  this means that the body of the subprogram is provided by the compiler itself,
  usually by means of an efficient code sequence, and that the user does not
  supply an explicit body for it. In an application program, the pragma may
  be applied to the following sets of names:


  * Rotate_Left, Rotate_Right, Shift_Left, Shift_Right, Shift_Right_Arithmetic.
    The corresponding subprogram declaration must have
    two formal parameters. The
    first one must be a signed integer type or a modular type with a binary
    modulus, and the second parameter must be of type Natural.
    The return type must be the same as the type of the first argument. The size
    of this type can only be 8, 16, 32, or 64.


  * Binary arithmetic operators: '+', '-', '*', '/'.
    The corresponding operator declaration must have parameters and result type
    that have the same root numeric type (for example, all three are long_float
    types). This simplifies the definition of operations that use type checking
    to perform dimensional checks:


  .. code-block:: ada

      type Distance is new Long_Float;
      type Time     is new Long_Float;
      type Velocity is new Long_Float;
      function "/" (D : Distance; T : Time)
        return Velocity;
      pragma Import (Intrinsic, "/");

    This common idiom is often programmed with a generic definition and an
    explicit body. The pragma makes it simpler to introduce such declarations.
    It incurs no overhead in compilation time or code size, because it is
    implemented as a single machine instruction.


  * General subprogram entities. This is used  to bind an Ada subprogram
    declaration to
    a compiler builtin by name with back-ends where such interfaces are
    available. A typical example is the set of ``__builtin`` functions
    exposed by the GCC back-end, as in the following example:


    .. code-block:: ada

         function builtin_sqrt (F : Float) return Float;
         pragma Import (Intrinsic, builtin_sqrt, "__builtin_sqrtf");

    Most of the GCC builtins are accessible this way, and as for other
    import conventions (e.g. C), it is the user's responsibility to ensure
    that the Ada subprogram profile matches the underlying builtin
    expectations.

.. index:: Stdcall
.. index:: Convention Stdcall

``Stdcall``
  This is relevant only to Windows implementations of GNAT,
  and specifies that the ``Stdcall`` calling sequence will be used,
  as defined by the NT API. Nevertheless, to ease building
  cross-platform bindings this convention will be handled as a ``C`` calling
  convention on non-Windows platforms.

.. index:: DLL
.. index:: Convention DLL


``DLL``
  This is equivalent to ``Stdcall``.

.. index:: Win32
.. index:: Convention Win32


``Win32``
  This is equivalent to ``Stdcall``.

.. index:: Stubbed
.. index:: Convention Stubbed


``Stubbed``
  This is a special convention that indicates that the compiler
  should provide a stub body that raises ``Program_Error``.

GNAT additionally provides a useful pragma ``Convention_Identifier``
that can be used to parameterize conventions and allow additional synonyms
to be specified. For example if you have legacy code in which the convention
identifier Fortran77 was used for Fortran, you can use the configuration
pragma:

.. code-block:: ada

     pragma Convention_Identifier (Fortran77, Fortran);

And from now on the identifier Fortran77 may be used as a convention
identifier (for example in an ``Import`` pragma) with the same
meaning as Fortran.


.. _Building_Mixed_Ada_and_C++_Programs:

Building Mixed Ada and C++ Programs
-----------------------------------

A programmer inexperienced with mixed-language development may find that
building an application containing both Ada and C++ code can be a
challenge.  This section gives a few hints that should make this task easier.

.. _Interfacing_to_C++:

Interfacing to C++
^^^^^^^^^^^^^^^^^^

GNAT supports interfacing with the G++ compiler (or any C++ compiler
generating code that is compatible with the G++ Application Binary
Interface ---see http://www.codesourcery.com/archives/cxx-abi).

Interfacing can be done at 3 levels: simple data, subprograms, and
classes. In the first two cases, GNAT offers a specific ``Convention C_Plus_Plus``
(or ``CPP``) that behaves exactly like ``Convention C``.
Usually, C++ mangles the names of subprograms. To generate proper mangled
names automatically, see :ref:`Generating_Ada_Bindings_for_C_and_C++_headers`).
This problem can also be addressed manually in two ways:

* by modifying the C++ code in order to force a C convention using
  the ``extern "C"`` syntax.

* by figuring out the mangled name (using e.g. ``nm``) and using it as the
  Link_Name argument of the pragma import.

Interfacing at the class level can be achieved by using the GNAT specific
pragmas such as ``CPP_Constructor``.  See the :title:`GNAT_Reference_Manual` for additional information.

.. _Linking_a_Mixed_C++_and_Ada_Program:

Linking a Mixed C++ & Ada Program
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Usually the linker of the C++ development system must be used to link
mixed applications because most C++ systems will resolve elaboration
issues (such as calling constructors on global class instances)
transparently during the link phase. GNAT has been adapted to ease the
use of a foreign linker for the last phase. Three cases can be
considered:


* Using GNAT and G++ (GNU C++ compiler) from the same GCC installation:
  The C++ linker can simply be called by using the C++ specific driver
  called ``g++``.

  Note that if the C++ code uses inline functions, you will need to
  compile your C++ code with the :switch:`-fkeep-inline-functions` switch in
  order to provide an existing function implementation that the Ada code can
  link with.

  .. code-block:: sh

    $ g++ -c -fkeep-inline-functions file1.C
    $ g++ -c -fkeep-inline-functions file2.C
    $ gnatmake ada_unit -largs file1.o file2.o --LINK=g++


* Using GNAT and G++ from two different GCC installations: If both
  compilers are on the :envvar`PATH`, the previous method may be used. It is
  important to note that environment variables such as
  :envvar:`C_INCLUDE_PATH`, :envvar:`GCC_EXEC_PREFIX`,
  :envvar:`BINUTILS_ROOT`, and
  :envvar:`GCC_ROOT` will affect both compilers
  at the same time and may make one of the two compilers operate
  improperly if set during invocation of the wrong compiler.  It is also
  very important that the linker uses the proper :file:`libgcc.a` GCC
  library -- that is, the one from the C++ compiler installation. The
  implicit link command as suggested in the ``gnatmake`` command
  from the former example can be replaced by an explicit link command with
  the full-verbosity option in order to verify which library is used:

  .. code-block:: sh

    $ gnatbind ada_unit
    $ gnatlink -v -v ada_unit file1.o file2.o --LINK=c++

  If there is a problem due to interfering environment variables, it can
  be worked around by using an intermediate script. The following example
  shows the proper script to use when GNAT has not been installed at its
  default location and g++ has been installed at its default location:

  .. code-block:: sh

    $ cat ./my_script
    #!/bin/sh
    unset BINUTILS_ROOT
    unset GCC_ROOT
    c++ $*
    $ gnatlink -v -v ada_unit file1.o file2.o --LINK=./my_script


* Using a non-GNU C++ compiler: The commands previously described can be
  used to insure that the C++ linker is used. Nonetheless, you need to add
  a few more parameters to the link command line, depending on the exception
  mechanism used.

  If the ``setjmp`` / ``longjmp`` exception mechanism is used, only the paths
  to the ``libgcc`` libraries are required:

  .. code-block:: sh

    $ cat ./my_script
    #!/bin/sh
    CC $* gcc -print-file-name=libgcc.a gcc -print-file-name=libgcc_eh.a
    $ gnatlink ada_unit file1.o file2.o --LINK=./my_script


  where CC is the name of the non-GNU C++ compiler.

  If the "zero cost" exception mechanism is used, and the platform
  supports automatic registration of exception tables (e.g., Solaris),
  paths to more objects are required:

  .. code-block:: sh

    $ cat ./my_script
    #!/bin/sh
    CC gcc -print-file-name=crtbegin.o $* \\
    gcc -print-file-name=libgcc.a gcc -print-file-name=libgcc_eh.a \\
    gcc -print-file-name=crtend.o
    $ gnatlink ada_unit file1.o file2.o --LINK=./my_script


  If the "zero cost exception" mechanism is used, and the platform
  doesn't support automatic registration of exception tables (e.g., HP-UX
  or AIX), the simple approach described above will not work and
  a pre-linking phase using GNAT will be necessary.


Another alternative is to use the :command:`gprbuild` multi-language builder
which has a large knowledge base and knows how to link Ada and C++ code
together automatically in most cases.

.. _A_Simple_Example:

A Simple Example
^^^^^^^^^^^^^^^^

The following example, provided as part of the GNAT examples, shows how
to achieve procedural interfacing between Ada and C++ in both
directions. The C++ class A has two methods. The first method is exported
to Ada by the means of an extern C wrapper function. The second method
calls an Ada subprogram. On the Ada side, The C++ calls are modelled by
a limited record with a layout comparable to the C++ class. The Ada
subprogram, in turn, calls the C++ method. So, starting from the C++
main program, the process passes back and forth between the two
languages.

Here are the compilation commands:

.. code-block:: sh

     $ gnatmake -c simple_cpp_interface
     $ g++ -c cpp_main.C
     $ g++ -c ex7.C
     $ gnatbind -n simple_cpp_interface
     $ gnatlink simple_cpp_interface -o cpp_main --LINK=g++ -lstdc++ ex7.o cpp_main.o

Here are the corresponding sources:

.. code-block:: cpp

     //cpp_main.C

     #include "ex7.h"

     extern "C" {
       void adainit (void);
       void adafinal (void);
       void method1 (A *t);
     }

     void method1 (A *t)
     {
       t->method1 ();
     }

     int main ()
     {
       A obj;
       adainit ();
       obj.method2 (3030);
       adafinal ();
     }

.. code-block:: cpp

     //ex7.h

     class Origin {
      public:
       int o_value;
     };
     class A : public Origin {
      public:
       void method1 (void);
       void method2 (int v);
       A();
       int   a_value;
     };

.. code-block:: cpp

     //ex7.C

     #include "ex7.h"
     #include <stdio.h>

     extern "C" { void ada_method2 (A *t, int v);}

     void A::method1 (void)
     {
       a_value = 2020;
       printf ("in A::method1, a_value = %d \\n",a_value);
     }

     void A::method2 (int v)
     {
        ada_method2 (this, v);
        printf ("in A::method2, a_value = %d \\n",a_value);
     }

     A::A(void)
     {
        a_value = 1010;
       printf ("in A::A, a_value = %d \\n",a_value);
     }

.. code-block:: ada

     -- simple_cpp_interface.ads
     with System;
     package Simple_Cpp_Interface is
        type A is limited
           record
              Vptr    : System.Address;
              O_Value : Integer;
              A_Value : Integer;
           end record;
        pragma Convention (C, A);

        procedure Method1 (This : in out A);
        pragma Import (C, Method1);

        procedure Ada_Method2 (This : in out A; V : Integer);
        pragma Export (C, Ada_Method2);

     end Simple_Cpp_Interface;

.. code-block:: ada

     -- simple_cpp_interface.adb
     package body Simple_Cpp_Interface is

        procedure Ada_Method2 (This : in out A; V : Integer) is
        begin
           Method1 (This);
           This.A_Value := V;
        end Ada_Method2;

     end Simple_Cpp_Interface;


.. _Interfacing_with_C++_constructors:

Interfacing with C++ constructors
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In order to interface with C++ constructors GNAT provides the
``pragma CPP_Constructor`` (see the :title:`GNAT_Reference_Manual`
for additional information).
In this section we present some common uses of C++ constructors
in mixed-languages programs in GNAT.

Let us assume that we need to interface with the following
C++ class:

.. code-block:: cpp

     class Root {
     public:
       int  a_value;
       int  b_value;
       virtual int Get_Value ();
       Root();              // Default constructor
       Root(int v);         // 1st non-default constructor
       Root(int v, int w);  // 2nd non-default constructor
     };

For this purpose we can write the following package spec (further
information on how to build this spec is available in
:ref:`Interfacing_with_C++_at_the_Class_Level` and
:ref:`Generating_Ada_Bindings_for_C_and_C++_headers`).

.. code-block:: ada

     with Interfaces.C; use Interfaces.C;
     package Pkg_Root is
       type Root is tagged limited record
          A_Value : int;
          B_Value : int;
       end record;
       pragma Import (CPP, Root);

       function Get_Value (Obj : Root) return int;
       pragma Import (CPP, Get_Value);

       function Constructor return Root;
       pragma Cpp_Constructor (Constructor, "_ZN4RootC1Ev");

       function Constructor (v : Integer) return Root;
       pragma Cpp_Constructor (Constructor, "_ZN4RootC1Ei");

       function Constructor (v, w : Integer) return Root;
       pragma Cpp_Constructor (Constructor, "_ZN4RootC1Eii");
     end Pkg_Root;

On the Ada side the constructor is represented by a function (whose
name is arbitrary) that returns the classwide type corresponding to
the imported C++ class. Although the constructor is described as a
function, it is typically a procedure with an extra implicit argument
(the object being initialized) at the implementation level. GNAT
issues the appropriate call, whatever it is, to get the object
properly initialized.

Constructors can only appear in the following contexts:

* On the right side of an initialization of an object of type ``T``.
* On the right side of an initialization of a record component of type ``T``.
* In an Ada 2005 limited aggregate.
* In an Ada 2005 nested limited aggregate.
* In an Ada 2005 limited aggregate that initializes an object built in
  place by an extended return statement.

In a declaration of an object whose type is a class imported from C++,
either the default C++ constructor is implicitly called by GNAT, or
else the required C++ constructor must be explicitly called in the
expression that initializes the object. For example:

.. code-block:: ada

     Obj1 : Root;
     Obj2 : Root := Constructor;
     Obj3 : Root := Constructor (v => 10);
     Obj4 : Root := Constructor (30, 40);

The first two declarations are equivalent: in both cases the default C++
constructor is invoked (in the former case the call to the constructor is
implicit, and in the latter case the call is explicit in the object
declaration). ``Obj3`` is initialized by the C++ non-default constructor
that takes an integer argument, and ``Obj4`` is initialized by the
non-default C++ constructor that takes two integers.

Let us derive the imported C++ class in the Ada side. For example:

.. code-block:: ada

     type DT is new Root with record
        C_Value : Natural := 2009;
     end record;

In this case the components DT inherited from the C++ side must be
initialized by a C++ constructor, and the additional Ada components
of type DT are initialized by GNAT. The initialization of such an
object is done either by default, or by means of a function returning
an aggregate of type DT, or by means of an extension aggregate.

.. code-block:: ada

     Obj5 : DT;
     Obj6 : DT := Function_Returning_DT (50);
     Obj7 : DT := (Constructor (30,40) with C_Value => 50);

The declaration of ``Obj5`` invokes the default constructors: the
C++ default constructor of the parent type takes care of the initialization
of the components inherited from Root, and GNAT takes care of the default
initialization of the additional Ada components of type DT (that is,
``C_Value`` is initialized to value 2009). The order of invocation of
the constructors is consistent with the order of elaboration required by
Ada and C++. That is, the constructor of the parent type is always called
before the constructor of the derived type.

Let us now consider a record that has components whose type is imported
from C++. For example:

.. code-block:: ada

     type Rec1 is limited record
        Data1 : Root := Constructor (10);
        Value : Natural := 1000;
     end record;

     type Rec2 (D : Integer := 20) is limited record
        Rec   : Rec1;
        Data2 : Root := Constructor (D, 30);
     end record;

The initialization of an object of type ``Rec2`` will call the
non-default C++ constructors specified for the imported components.
For example:

.. code-block:: ada

     Obj8 : Rec2 (40);

Using Ada 2005 we can use limited aggregates to initialize an object
invoking C++ constructors that differ from those specified in the type
declarations. For example:

.. code-block:: ada

     Obj9 : Rec2 := (Rec => (Data1 => Constructor (15, 16),
                             others => <>),
                     others => <>);

The above declaration uses an Ada 2005 limited aggregate to
initialize ``Obj9``, and the C++ constructor that has two integer
arguments is invoked to initialize the ``Data1`` component instead
of the constructor specified in the declaration of type ``Rec1``. In
Ada 2005 the box in the aggregate indicates that unspecified components
are initialized using the expression (if any) available in the component
declaration. That is, in this case discriminant ``D`` is initialized
to value ``20``, ``Value`` is initialized to value 1000, and the
non-default C++ constructor that handles two integers takes care of
initializing component ``Data2`` with values ``20,30``.

In Ada 2005 we can use the extended return statement to build the Ada
equivalent to C++ non-default constructors. For example:

.. code-block:: ada

     function Constructor (V : Integer) return Rec2 is
     begin
        return Obj : Rec2 := (Rec => (Data1  => Constructor (V, 20),
                                      others => <>),
                              others => <>) do
           --  Further actions required for construction of
           --  objects of type Rec2
           ...
        end record;
     end Constructor;

In this example the extended return statement construct is used to
build in place the returned object whose components are initialized
by means of a limited aggregate. Any further action associated with
the constructor can be placed inside the construct.

.. _Interfacing_with_C++_at_the_Class_Level:

Interfacing with C++ at the Class Level
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In this section we demonstrate the GNAT features for interfacing with
C++ by means of an example making use of Ada 2005 abstract interface
types. This example consists of a classification of animals; classes
have been used to model our main classification of animals, and
interfaces provide support for the management of secondary
classifications. We first demonstrate a case in which the types and
constructors are defined on the C++ side and imported from the Ada
side, and latter the reverse case.

The root of our derivation will be the ``Animal`` class, with a
single private attribute (the ``Age`` of the animal), a constructor,
and two public primitives to set and get the value of this attribute.

.. code-block:: cpp

     class Animal {
      public:
        virtual void Set_Age (int New_Age);
        virtual int Age ();
        Animal() {Age_Count = 0;};
      private:
        int Age_Count;
     };

Abstract interface types are defined in C++ by means of classes with pure
virtual functions and no data members. In our example we will use two
interfaces that provide support for the common management of ``Carnivore``
and ``Domestic`` animals:

.. code-block:: cpp

     class Carnivore {
     public:
        virtual int Number_Of_Teeth () = 0;
     };

     class Domestic {
     public:
        virtual void Set_Owner (char* Name) = 0;
     };

Using these declarations, we can now say that a ``Dog`` is an animal that is
both Carnivore and Domestic, that is:

.. code-block:: cpp

     class Dog : Animal, Carnivore, Domestic {
      public:
        virtual int  Number_Of_Teeth ();
        virtual void Set_Owner (char* Name);

        Dog(); // Constructor
      private:
        int  Tooth_Count;
        char *Owner;
     };

In the following examples we will assume that the previous declarations are
located in a file named :file:`animals.h`. The following package demonstrates
how to import these C++ declarations from the Ada side:

.. code-block:: ada

     with Interfaces.C.Strings; use Interfaces.C.Strings;
     package Animals is
       type Carnivore is limited interface;
       pragma Convention (C_Plus_Plus, Carnivore);
       function Number_Of_Teeth (X : Carnivore)
          return Natural is abstract;

       type Domestic is limited interface;
       pragma Convention (C_Plus_Plus, Domestic);
       procedure Set_Owner
         (X    : in out Domestic;
          Name : Chars_Ptr) is abstract;

       type Animal is tagged limited record
         Age : Natural;
       end record;
       pragma Import (C_Plus_Plus, Animal);

       procedure Set_Age (X : in out Animal; Age : Integer);
       pragma Import (C_Plus_Plus, Set_Age);

       function Age (X : Animal) return Integer;
       pragma Import (C_Plus_Plus, Age);

       function New_Animal return Animal;
       pragma CPP_Constructor (New_Animal);
       pragma Import (CPP, New_Animal, "_ZN6AnimalC1Ev");

       type Dog is new Animal and Carnivore and Domestic with record
         Tooth_Count : Natural;
         Owner       : Chars_Ptr;
       end record;
       pragma Import (C_Plus_Plus, Dog);

       function Number_Of_Teeth (A : Dog) return Natural;
       pragma Import (C_Plus_Plus, Number_Of_Teeth);

       procedure Set_Owner (A : in out Dog; Name : Chars_Ptr);
       pragma Import (C_Plus_Plus, Set_Owner);

       function New_Dog return Dog;
       pragma CPP_Constructor (New_Dog);
       pragma Import (CPP, New_Dog, "_ZN3DogC2Ev");
     end Animals;

Thanks to the compatibility between GNAT run-time structures and the C++ ABI,
interfacing with these C++ classes is easy. The only requirement is that all
the primitives and components must be declared exactly in the same order in
the two languages.

Regarding the abstract interfaces, we must indicate to the GNAT compiler by
means of a ``pragma Convention (C_Plus_Plus)``, the convention used to pass
the arguments to the called primitives will be the same as for C++. For the
imported classes we use ``pragma Import`` with convention ``C_Plus_Plus``
to indicate that they have been defined on the C++ side; this is required
because the dispatch table associated with these tagged types will be built
in the C++ side and therefore will not contain the predefined Ada primitives
which Ada would otherwise expect.

As the reader can see there is no need to indicate the C++ mangled names
associated with each subprogram because it is assumed that all the calls to
these primitives will be dispatching calls. The only exception is the
constructor, which must be registered with the compiler by means of
``pragma CPP_Constructor`` and needs to provide its associated C++
mangled name because the Ada compiler generates direct calls to it.

With the above packages we can now declare objects of type Dog on the Ada side
and dispatch calls to the corresponding subprograms on the C++ side. We can
also extend the tagged type Dog with further fields and primitives, and
override some of its C++ primitives on the Ada side. For example, here we have
a type derivation defined on the Ada side that inherits all the dispatching
primitives of the ancestor from the C++ side.

.. code-block:: ada

     with Animals; use Animals;
     package Vaccinated_Animals is
       type Vaccinated_Dog is new Dog with null record;
       function Vaccination_Expired (A : Vaccinated_Dog) return Boolean;
     end Vaccinated_Animals;

It is important to note that, because of the ABI compatibility, the programmer
does not need to add any further information to indicate either the object
layout or the dispatch table entry associated with each dispatching operation.

Now let us define all the types and constructors on the Ada side and export
them to C++, using the same hierarchy of our previous example:

.. code-block:: ada

     with Interfaces.C.Strings;
     use Interfaces.C.Strings;
     package Animals is
       type Carnivore is limited interface;
       pragma Convention (C_Plus_Plus, Carnivore);
       function Number_Of_Teeth (X : Carnivore)
          return Natural is abstract;

       type Domestic is limited interface;
       pragma Convention (C_Plus_Plus, Domestic);
       procedure Set_Owner
         (X    : in out Domestic;
          Name : Chars_Ptr) is abstract;

       type Animal is tagged record
         Age : Natural;
       end record;
       pragma Convention (C_Plus_Plus, Animal);

       procedure Set_Age (X : in out Animal; Age : Integer);
       pragma Export (C_Plus_Plus, Set_Age);

       function Age (X : Animal) return Integer;
       pragma Export (C_Plus_Plus, Age);

       function New_Animal return Animal'Class;
       pragma Export (C_Plus_Plus, New_Animal);

       type Dog is new Animal and Carnivore and Domestic with record
         Tooth_Count : Natural;
         Owner       : String (1 .. 30);
       end record;
       pragma Convention (C_Plus_Plus, Dog);

       function Number_Of_Teeth (A : Dog) return Natural;
       pragma Export (C_Plus_Plus, Number_Of_Teeth);

       procedure Set_Owner (A : in out Dog; Name : Chars_Ptr);
       pragma Export (C_Plus_Plus, Set_Owner);

       function New_Dog return Dog'Class;
       pragma Export (C_Plus_Plus, New_Dog);
     end Animals;

Compared with our previous example the only differences are the use of
``pragma Convention`` (instead of ``pragma Import``), and the use of
``pragma Export`` to indicate to the GNAT compiler that the primitives will
be available to C++. Thanks to the ABI compatibility, on the C++ side there is
nothing else to be done; as explained above, the only requirement is that all
the primitives and components are declared in exactly the same order.

For completeness, let us see a brief C++ main program that uses the
declarations available in :file:`animals.h` (presented in our first example) to
import and use the declarations from the Ada side, properly initializing and
finalizing the Ada run-time system along the way:

.. code-block:: cpp

     #include "animals.h"
     #include <iostream>
     using namespace std;

     void Check_Carnivore (Carnivore *obj) {...}
     void Check_Domestic (Domestic *obj)   {...}
     void Check_Animal (Animal *obj)       {...}
     void Check_Dog (Dog *obj)             {...}

     extern "C" {
       void adainit (void);
       void adafinal (void);
       Dog* new_dog ();
     }

     void test ()
     {
       Dog *obj = new_dog();  // Ada constructor
       Check_Carnivore (obj); // Check secondary DT
       Check_Domestic (obj);  // Check secondary DT
       Check_Animal (obj);    // Check primary DT
       Check_Dog (obj);       // Check primary DT
     }

     int main ()
     {
       adainit ();  test();  adafinal ();
       return 0;
     }

.. _Generating_Ada_Bindings_for_C_and_C++_headers:

Generating Ada Bindings for C and C++ headers
---------------------------------------------

.. index:: Binding generation (for C and C++ headers)
.. index:: C headers (binding generation)
.. index:: C++ headers (binding generation)

GNAT includes a binding generator for C and C++ headers which is
intended to do 95% of the tedious work of generating Ada specs from C
or C++ header files.

Note that this capability is not intended to generate 100% correct Ada specs,
and will is some cases require manual adjustments, although it can often
be used out of the box in practice.

Some of the known limitations include:

* only very simple character constant macros are translated into Ada
  constants. Function macros (macros with arguments) are partially translated
  as comments, to be completed manually if needed.
* some extensions (e.g. vector types) are not supported
* pointers to pointers or complex structures are mapped to System.Address
* identifiers with identical name (except casing) will generate compilation
  errors (e.g. ``shm_get`` vs ``SHM_GET``).

The code is generated using Ada 2012 syntax, which makes it easier to interface
with other languages. In most cases you can still use the generated binding
even if your code is compiled using earlier versions of Ada (e.g. ``-gnat95``).

.. _Running_the_binding_generator:

Running the Binding Generator
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The binding generator is part of the ``gcc`` compiler and can be
invoked via the :switch:`-fdump-ada-spec` switch, which will generate Ada
spec files for the header files specified on the command line, and all
header files needed by these files transitively. For example:

.. code-block:: sh

      $ g++ -c -fdump-ada-spec -C /usr/include/time.h
      $ gcc -c *.ads

will generate, under GNU/Linux, the following files: :file:`time_h.ads`,
:file:`bits_time_h.ads`, :file:`stddef_h.ads`, :file:`bits_types_h.ads` which
correspond to the files :file:`/usr/include/time.h`,
:file:`/usr/include/bits/time.h`, etc..., and will then compile these Ada specs
in Ada 2005 mode.

The :switch:`-C` switch tells ``gcc`` to extract comments from headers,
and will attempt to generate corresponding Ada comments.

If you want to generate a single Ada file and not the transitive closure, you
can use instead the :switch:`-fdump-ada-spec-slim` switch.

You can optionally specify a parent unit, of which all generated units will
be children, using :switch:`-fada-spec-parent={unit}`.

Note that we recommend when possible to use the *g++* driver to
generate bindings, even for most C headers, since this will in general
generate better Ada specs. For generating bindings for C++ headers, it is
mandatory to use the *g++* command, or *gcc -x c++* which
is equivalent in this case. If *g++* cannot work on your C headers
because of incompatibilities between C and C++, then you can fallback to
``gcc`` instead.

For an example of better bindings generated from the C++ front-end,
the name of the parameters (when available) are actually ignored by the C
front-end. Consider the following C header:

.. code-block:: c

     extern void foo (int variable);

with the C front-end, ``variable`` is ignored, and the above is handled as:

.. code-block:: c

     extern void foo (int);

generating a generic:

.. code-block:: ada

     procedure foo (param1 : int);

with the C++ front-end, the name is available, and we generate:

.. code-block:: ada

     procedure foo (variable : int);

In some cases, the generated bindings will be more complete or more meaningful
when defining some macros, which you can do via the :switch:`-D` switch. This
is for example the case with :file:`Xlib.h` under GNU/Linux:

.. code-block:: sh

      $ g++ -c -fdump-ada-spec -DXLIB_ILLEGAL_ACCESS -C /usr/include/X11/Xlib.h

The above will generate more complete bindings than a straight call without
the :switch:`-DXLIB_ILLEGAL_ACCESS` switch.

In other cases, it is not possible to parse a header file in a stand-alone
manner, because other include files need to be included first. In this
case, the solution is to create a small header file including the needed
``#include`` and possible ``#define`` directives. For example, to
generate Ada bindings for :file:`readline/readline.h`, you need to first
include :file:`stdio.h`, so you can create a file with the following two
lines in e.g. :file:`readline1.h`:

.. code-block:: cpp

      #include <stdio.h>
      #include <readline/readline.h>

and then generate Ada bindings from this file:

.. code-block:: sh

      $ g++ -c -fdump-ada-spec readline1.h


.. _Generating_bindings_for_C++_headers:

Generating Bindings for C++ Headers
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Generating bindings for C++ headers is done using the same options, always
with the *g++* compiler. Note that generating Ada spec from C++ headers is a
much more complex job and support for C++ headers is much more limited that
support for C headers. As a result, you will need to modify the resulting
bindings by hand more extensively when using C++ headers.

In this mode, C++ classes will be mapped to Ada tagged types, constructors
will be mapped using the ``CPP_Constructor`` pragma, and when possible,
multiple inheritance of abstract classes will be mapped to Ada interfaces
(see the *Interfacing to C++* section in the :title:`GNAT Reference Manual`
for additional information on interfacing to C++).

For example, given the following C++ header file:

.. code-block:: cpp

       class Carnivore {
       public:
          virtual int Number_Of_Teeth () = 0;
       };

       class Domestic {
       public:
          virtual void Set_Owner (char* Name) = 0;
       };

       class Animal {
       public:
         int Age_Count;
         virtual void Set_Age (int New_Age);
       };

       class Dog : Animal, Carnivore, Domestic {
        public:
         int  Tooth_Count;
         char *Owner;

         virtual int  Number_Of_Teeth ();
         virtual void Set_Owner (char* Name);

         Dog();
       };

The corresponding Ada code is generated:

.. code-block:: ada

         package Class_Carnivore is
           type Carnivore is limited interface;
           pragma Import (CPP, Carnivore);

           function Number_Of_Teeth (this : access Carnivore) return int is abstract;
         end;
         use Class_Carnivore;

         package Class_Domestic is
           type Domestic is limited interface;
           pragma Import (CPP, Domestic);

           procedure Set_Owner
             (this : access Domestic;
              Name : Interfaces.C.Strings.chars_ptr) is abstract;
         end;
         use Class_Domestic;

         package Class_Animal is
           type Animal is tagged limited record
             Age_Count : aliased int;
           end record;
           pragma Import (CPP, Animal);

           procedure Set_Age (this : access Animal; New_Age : int);
           pragma Import (CPP, Set_Age, "_ZN6Animal7Set_AgeEi");
         end;
         use Class_Animal;

         package Class_Dog is
           type Dog is new Animal and Carnivore and Domestic with record
             Tooth_Count : aliased int;
             Owner : Interfaces.C.Strings.chars_ptr;
           end record;
           pragma Import (CPP, Dog);

           function Number_Of_Teeth (this : access Dog) return int;
           pragma Import (CPP, Number_Of_Teeth, "_ZN3Dog15Number_Of_TeethEv");

           procedure Set_Owner
             (this : access Dog; Name : Interfaces.C.Strings.chars_ptr);
           pragma Import (CPP, Set_Owner, "_ZN3Dog9Set_OwnerEPc");

           function New_Dog return Dog;
           pragma CPP_Constructor (New_Dog);
           pragma Import (CPP, New_Dog, "_ZN3DogC1Ev");
         end;
         use Class_Dog;


.. _Switches_for_Ada_Binding_Generation:

Switches
^^^^^^^^

.. index:: -fdump-ada-spec (gcc)

:switch:`-fdump-ada-spec`
  Generate Ada spec files for the given header files transitively (including
  all header files that these headers depend upon).

.. index:: -fdump-ada-spec-slim (gcc)

:switch:`-fdump-ada-spec-slim`
  Generate Ada spec files for the header files specified on the command line
  only.

.. index:: -fada-spec-parent (gcc)

:switch:`-fada-spec-parent={unit}`
  Specifies that all files generated by :switch:`-fdump-ada-spec` are
  to be child units of the specified parent unit.

.. index:: -C (gcc)

:switch:`-C`
  Extract comments from headers and generate Ada comments in the Ada spec files.

.. _Generating_C_Headers_for_Ada_Specifications:

Generating C Headers for Ada Specifications
-------------------------------------------

.. index:: Binding generation (for Ada specs)
.. index:: C headers (binding generation)

GNAT includes a C header generator for Ada specifications which supports
Ada types that have a direct mapping to C types. This includes in particular
support for:

* Scalar types
* Constrained arrays
* Records (untagged)
* Composition of the above types
* Constant declarations
* Object declarations
* Subprogram declarations

Running the C Header Generator
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The C header generator is part of the GNAT compiler and can be invoked via
the :switch:`-gnatceg` combination of switches, which will generate a :file:`.h`
file corresponding to the given input file (Ada spec or body). Note that
only spec files are processed in any case, so giving a spec or a body file
as input is equivalent. For example:

.. code-block:: sh

   $ gcc -c -gnatceg pack1.ads

will generate a self-contained file called :file:`pack1.h` including
common definitions from the Ada Standard package, followed by the
definitions included in :file:`pack1.ads`, as well as all the other units
withed by this file.

For instance, given the following Ada files:

.. code-block:: ada

   package Pack2 is
      type Int is range 1 .. 10;
   end Pack2;

.. code-block:: ada

   with Pack2;

   package Pack1 is
      type Rec is record
         Field1, Field2 : Pack2.Int;
      end record;

      Global : Rec := (1, 2);

      procedure Proc1 (R : Rec);
      procedure Proc2 (R : in out Rec);
   end Pack1;

The above ``gcc`` command will generate the following :file:`pack1.h` file:

.. code-block:: c

   /* Standard definitions skipped */
   #ifndef PACK2_ADS
   #define PACK2_ADS
   typedef short_short_integer pack2__TintB;
   typedef pack2__TintB pack2__int;
   #endif /* PACK2_ADS */

   #ifndef PACK1_ADS
   #define PACK1_ADS
   typedef struct _pack1__rec {
     pack2__int field1;
     pack2__int field2;
   } pack1__rec;
   extern pack1__rec pack1__global;
   extern void pack1__proc1(const pack1__rec r);
   extern void pack1__proc2(pack1__rec *r);
   #endif /* PACK1_ADS */

You can then ``include`` :file:`pack1.h` from a C source file and use the types,
call subprograms, reference objects, and constants.

.. _GNAT_and_Other_Compilation_Models:

GNAT and Other Compilation Models
=================================

This section compares the GNAT model with the approaches taken in
other environents, first the C/C++ model and then the mechanism that
has been used in other Ada systems, in particular those traditionally
used for Ada 83.

.. _Comparison_between_GNAT_and_C/C++_Compilation_Models:

Comparison between GNAT and C/C++ Compilation Models
----------------------------------------------------

The GNAT model of compilation is close to the C and C++ models. You can
think of Ada specs as corresponding to header files in C. As in C, you
don't need to compile specs; they are compiled when they are used. The
Ada |with| is similar in effect to the ``#include`` of a C
header.

One notable difference is that, in Ada, you may compile specs separately
to check them for semantic and syntactic accuracy. This is not always
possible with C headers because they are fragments of programs that have
less specific syntactic or semantic rules.

The other major difference is the requirement for running the binder,
which performs two important functions. First, it checks for
consistency. In C or C++, the only defense against assembling
inconsistent programs lies outside the compiler, in a makefile, for
example. The binder satisfies the Ada requirement that it be impossible
to construct an inconsistent program when the compiler is used in normal
mode.

.. index:: Elaboration order control

The other important function of the binder is to deal with elaboration
issues. There are also elaboration issues in C++ that are handled
automatically. This automatic handling has the advantage of being
simpler to use, but the C++ programmer has no control over elaboration.
Where ``gnatbind`` might complain there was no valid order of
elaboration, a C++ compiler would simply construct a program that
malfunctioned at run time.

.. _Comparison_between_GNAT_and_Conventional_Ada_Library_Models:

Comparison between GNAT and Conventional Ada Library Models
-----------------------------------------------------------

This section is intended for Ada programmers who have
used an Ada compiler implementing the traditional Ada library
model, as described in the Ada Reference Manual.

.. index:: GNAT library

In GNAT, there is no 'library' in the normal sense. Instead, the set of
source files themselves acts as the library. Compiling Ada programs does
not generate any centralized information, but rather an object file and
a ALI file, which are of interest only to the binder and linker.
In a traditional system, the compiler reads information not only from
the source file being compiled, but also from the centralized library.
This means that the effect of a compilation depends on what has been
previously compiled. In particular:

* When a unit is |withed|, the unit seen by the compiler corresponds
  to the version of the unit most recently compiled into the library.

* Inlining is effective only if the necessary body has already been
  compiled into the library.

* Compiling a unit may obsolete other units in the library.

In GNAT, compiling one unit never affects the compilation of any other
units because the compiler reads only source files. Only changes to source
files can affect the results of a compilation. In particular:

* When a unit is |withed|, the unit seen by the compiler corresponds
  to the source version of the unit that is currently accessible to the
  compiler.

  .. index:: Inlining

* Inlining requires the appropriate source files for the package or
  subprogram bodies to be available to the compiler. Inlining is always
  effective, independent of the order in which units are compiled.

* Compiling a unit never affects any other compilations. The editing of
  sources may cause previous compilations to be out of date if they
  depended on the source file being modified.

The most important result of these differences is that order of compilation
is never significant in GNAT. There is no situation in which one is
required to do one compilation before another. What shows up as order of
compilation requirements in the traditional Ada library becomes, in
GNAT, simple source dependencies; in other words, there is only a set
of rules saying what source files must be present when a file is
compiled.


.. _Using_GNAT_Files_with_External_Tools:

Using GNAT Files with External Tools
====================================

This section explains how files that are produced by GNAT may be
used with tools designed for other languages.


.. _Using_Other_Utility_Programs_with_GNAT:

Using Other Utility Programs with GNAT
--------------------------------------

The object files generated by GNAT are in standard system format and in
particular the debugging information uses this format. This means
programs generated by GNAT can be used with existing utilities that
depend on these formats.

In general, any utility program that works with C will also often work with
Ada programs generated by GNAT. This includes software utilities such as
gprof (a profiling program), gdb (the FSF debugger), and utilities such
as Purify.


.. _The_External_Symbol_Naming_Scheme_of_GNAT:

The External Symbol Naming Scheme of GNAT
-----------------------------------------

In order to interpret the output from GNAT, when using tools that are
originally intended for use with other languages, it is useful to
understand the conventions used to generate link names from the Ada
entity names.

All link names are in all lowercase letters. With the exception of library
procedure names, the mechanism used is simply to use the full expanded
Ada name with dots replaced by double underscores. For example, suppose
we have the following package spec:

.. code-block:: ada

     package QRS is
        MN : Integer;
     end QRS;

.. index:: pragma Export

The variable ``MN`` has a full expanded Ada name of ``QRS.MN``, so
the corresponding link name is ``qrs__mn``.
Of course if a ``pragma Export`` is used this may be overridden:

.. code-block:: ada

     package Exports is
        Var1 : Integer;
        pragma Export (Var1, C, External_Name => "var1_name");
        Var2 : Integer;
        pragma Export (Var2, C, Link_Name => "var2_link_name");
     end Exports;

In this case, the link name for ``Var1`` is whatever link name the
C compiler would assign for the C function ``var1_name``. This typically
would be either ``var1_name`` or ``_var1_name``, depending on operating
system conventions, but other possibilities exist. The link name for
``Var2`` is ``var2_link_name``, and this is not operating system
dependent.

One exception occurs for library level procedures. A potential ambiguity
arises between the required name ``_main`` for the C main program,
and the name we would otherwise assign to an Ada library level procedure
called ``Main`` (which might well not be the main program).

To avoid this ambiguity, we attach the prefix ``_ada_`` to such
names. So if we have a library level procedure such as:

.. code-block:: ada

     procedure Hello (S : String);

the external name of this procedure will be ``_ada_hello``.
