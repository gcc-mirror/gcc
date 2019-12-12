.. _Implementation_Defined_Characteristics:

**************************************
Implementation Defined Characteristics
**************************************

In addition to the implementation dependent pragmas and attributes, and the
implementation advice, there are a number of other Ada features that are
potentially implementation dependent and are designated as
implementation-defined. These are mentioned throughout the Ada Reference
Manual, and are summarized in Annex M.

A requirement for conforming Ada compilers is that they provide
documentation describing how the implementation deals with each of these
issues.  In this chapter you will find each point in Annex M listed,
followed by a description of how GNAT
handles the implementation dependence.

You can use this chapter as a guide to minimizing implementation
dependent features in your programs if portability to other compilers
and other operating systems is an important consideration.  The numbers
in each entry below correspond to the paragraph numbers in the Ada
Reference Manual.

*
  "Whether or not each recommendation given in Implementation
  Advice is followed.  See 1.1.2(37)."

See :ref:`Implementation_Advice`.

*
  "Capacity limitations of the implementation.  See 1.1.3(3)."

The complexity of programs that can be processed is limited only by the
total amount of available virtual memory, and disk space for the
generated object files.

*
  "Variations from the standard that are impractical to avoid
  given the implementation's execution environment.  See 1.1.3(6)."

There are no variations from the standard.

*
  "Which code_statements cause external
  interactions.  See 1.1.3(10)."

Any *code_statement* can potentially cause external interactions.

*
  "The coded representation for the text of an Ada
  program.  See 2.1(4)."

See separate section on source representation.

*
  "The control functions allowed in comments.  See 2.1(14)."

See separate section on source representation.

*
  "The representation for an end of line.  See 2.2(2)."

See separate section on source representation.

*
  "Maximum supported line length and lexical element
  length.  See 2.2(15)."

The maximum line length is 255 characters and the maximum length of
a lexical element is also 255 characters. This is the default setting
if not overridden by the use of compiler switch *-gnaty* (which
sets the maximum to 79) or *-gnatyMnn* which allows the maximum
line length to be specified to be any value up to 32767. The maximum
length of a lexical element is the same as the maximum line length.

*
  "Implementation defined pragmas.  See 2.8(14)."

See :ref:`Implementation_Defined_Pragmas`.

*
  "Effect of pragma ``Optimize``.  See 2.8(27)."

Pragma ``Optimize``, if given with a ``Time`` or ``Space``
parameter, checks that the optimization flag is set, and aborts if it is
not.

*
  "The sequence of characters of the value returned by
  ``S'Image`` when some of the graphic characters of
  ``S'Wide_Image`` are not defined in ``Character``.  See
  3.5(37)."

The sequence of characters is as defined by the wide character encoding
method used for the source.  See section on source representation for
further details.

*
  "The predefined integer types declared in
  ``Standard``.  See 3.5.4(25)."

====================== =======================================
Type                   Representation
====================== =======================================
*Short_Short_Integer*  8 bit signed
*Short_Integer*        (Short) 16 bit signed
*Integer*              32 bit signed
*Long_Integer*         64 bit signed (on most 64 bit targets,
                       depending on the C definition of long).
                       32 bit signed (all other targets)
*Long_Long_Integer*    64 bit signed
====================== =======================================

*
  "Any nonstandard integer types and the operators defined
  for them.  See 3.5.4(26)."

There are no nonstandard integer types.

*
  "Any nonstandard real types and the operators defined for
  them.  See 3.5.6(8)."

There are no nonstandard real types.

*
  "What combinations of requested decimal precision and range
  are supported for floating point types.  See 3.5.7(7)."

The precision and range is as defined by the IEEE standard.

*
  "The predefined floating point types declared in
  ``Standard``.  See 3.5.7(16)."

====================== ====================================================
Type                   Representation
====================== ====================================================
*Short_Float*          32 bit IEEE short
*Float*                (Short) 32 bit IEEE short
*Long_Float*           64 bit IEEE long
*Long_Long_Float*      64 bit IEEE long (80 bit IEEE long on x86 processors)
====================== ====================================================

*
  "The small of an ordinary fixed point type.  See 3.5.9(8)."

``Fine_Delta`` is 2**(-63)

*
  "What combinations of small, range, and digits are
  supported for fixed point types.  See 3.5.9(10)."

Any combinations are permitted that do not result in a small less than
``Fine_Delta`` and do not result in a mantissa larger than 63 bits.
If the mantissa is larger than 53 bits on machines where Long_Long_Float
is 64 bits (true of all architectures except ia32), then the output from
Text_IO is accurate to only 53 bits, rather than the full mantissa.  This
is because floating-point conversions are used to convert fixed point.


*
  "The result of ``Tags.Expanded_Name`` for types declared
  within an unnamed *block_statement*.  See 3.9(10)."

Block numbers of the form :samp:`B{nnn}`, where *nnn* is a
decimal integer are allocated.

*
  "Implementation-defined attributes.  See 4.1.4(12)."

See :ref:`Implementation_Defined_Attributes`.

*
  "Any implementation-defined time types.  See 9.6(6)."

There are no implementation-defined time types.

*
  "The time base associated with relative delays."

See 9.6(20).  The time base used is that provided by the C library
function ``gettimeofday``.

*
  "The time base of the type ``Calendar.Time``.  See
  9.6(23)."

The time base used is that provided by the C library function
``gettimeofday``.

*
  "The time zone used for package ``Calendar``
  operations.  See 9.6(24)."

The time zone used by package ``Calendar`` is the current system time zone
setting for local time, as accessed by the C library function
``localtime``.

*
  "Any limit on *delay_until_statements* of
  *select_statements*.  See 9.6(29)."

There are no such limits.

*
  "Whether or not two non-overlapping parts of a composite
  object are independently addressable, in the case where packing, record
  layout, or ``Component_Size`` is specified for the object.  See
  9.10(1)."

Separate components are independently addressable if they do not share
overlapping storage units.

*
  "The representation for a compilation.  See 10.1(2)."

A compilation is represented by a sequence of files presented to the
compiler in a single invocation of the *gcc* command.

*
  "Any restrictions on compilations that contain multiple
  compilation_units.  See 10.1(4)."

No single file can contain more than one compilation unit, but any
sequence of files can be presented to the compiler as a single
compilation.

*
  "The mechanisms for creating an environment and for adding
  and replacing compilation units.  See 10.1.4(3)."

See separate section on compilation model.

*
  "The manner of explicitly assigning library units to a
  partition.  See 10.2(2)."

If a unit contains an Ada main program, then the Ada units for the partition
are determined by recursive application of the rules in the Ada Reference
Manual section 10.2(2-6).  In other words, the Ada units will be those that
are needed by the main program, and then this definition of need is applied
recursively to those units, and the partition contains the transitive
closure determined by this relationship.  In short, all the necessary units
are included, with no need to explicitly specify the list.  If additional
units are required, e.g., by foreign language units, then all units must be
mentioned in the context clause of one of the needed Ada units.

If the partition contains no main program, or if the main program is in
a language other than Ada, then GNAT
provides the binder options *-z* and *-n* respectively, and in
this case a list of units can be explicitly supplied to the binder for
inclusion in the partition (all units needed by these units will also
be included automatically).  For full details on the use of these
options, refer to *GNAT Make Program gnatmake* in the
:title:`GNAT User's Guide`.

*
  "The implementation-defined means, if any, of specifying
  which compilation units are needed by a given compilation unit.  See
  10.2(2)."

The units needed by a given compilation unit are as defined in
the Ada Reference Manual section 10.2(2-6).  There are no
implementation-defined pragmas or other implementation-defined
means for specifying needed units.

*
  "The manner of designating the main subprogram of a
  partition.  See 10.2(7)."

The main program is designated by providing the name of the
corresponding :file:`ALI` file as the input parameter to the binder.

*
  "The order of elaboration of *library_items*.  See
  10.2(18)."

The first constraint on ordering is that it meets the requirements of
Chapter 10 of the Ada Reference Manual.  This still leaves some
implementation dependent choices, which are resolved by first
elaborating bodies as early as possible (i.e., in preference to specs
where there is a choice), and second by evaluating the immediate with
clauses of a unit to determine the probably best choice, and
third by elaborating in alphabetical order of unit names
where a choice still remains.

*
  "Parameter passing and function return for the main
  subprogram.  See 10.2(21)."

The main program has no parameters.  It may be a procedure, or a function
returning an integer type.  In the latter case, the returned integer
value is the return code of the program (overriding any value that
may have been set by a call to ``Ada.Command_Line.Set_Exit_Status``).

*
  "The mechanisms for building and running partitions.  See
  10.2(24)."

GNAT itself supports programs with only a single partition.  The GNATDIST
tool provided with the GLADE package (which also includes an implementation
of the PCS) provides a completely flexible method for building and running
programs consisting of multiple partitions.  See the separate GLADE manual
for details.

*
  "The details of program execution, including program
  termination.  See 10.2(25)."

See separate section on compilation model.

*
  "The semantics of any non-active partitions supported by the
  implementation.  See 10.2(28)."

Passive partitions are supported on targets where shared memory is
provided by the operating system.  See the GLADE reference manual for
further details.

*
  "The information returned by ``Exception_Message``.  See
  11.4.1(10)."

Exception message returns the null string unless a specific message has
been passed by the program.

*
  "The result of ``Exceptions.Exception_Name`` for types
  declared within an unnamed *block_statement*.  See 11.4.1(12)."

Blocks have implementation defined names of the form :samp:`B{nnn}`
where *nnn* is an integer.

*
  "The information returned by
  ``Exception_Information``.  See 11.4.1(13)."

``Exception_Information`` returns a string in the following format::

  *Exception_Name:* nnnnn
  *Message:* mmmmm
  *PID:* ppp
  *Load address:* 0xhhhh
  *Call stack traceback locations:*
  0xhhhh 0xhhhh 0xhhhh ... 0xhhh

where

  *  ``nnnn`` is the fully qualified name of the exception in all upper
     case letters. This line is always present.

  *  ``mmmm`` is the message (this line present only if message is non-null)

  *  ``ppp`` is the Process Id value as a decimal integer (this line is
     present only if the Process Id is nonzero). Currently we are
     not making use of this field.

  *  The Load address line, the Call stack traceback locations line and the
     following values are present only if at least one traceback location was
     recorded. The Load address indicates the address at which the main executable
     was loaded; this line may not be present if operating system hasn't relocated
     the main executable. The values are given in C style format, with lower case
     letters for a-f, and only as many digits present as are necessary.
     The line terminator sequence at the end of each line, including
     the last line is a single ``LF`` character (``16#0A#``).

*
  "Implementation-defined check names.  See 11.5(27)."

The implementation defined check names include Alignment_Check,
Atomic_Synchronization, Duplicated_Tag_Check, Container_Checks,
Tampering_Check, Predicate_Check, and Validity_Check. In addition, a user
program can add implementation-defined check names by means of the pragma
Check_Name. See the description of pragma ``Suppress`` for full details.

*
  "The interpretation of each aspect of representation.  See
  13.1(20)."

See separate section on data representations.

*
  "Any restrictions placed upon representation items.  See
  13.1(20)."

See separate section on data representations.

*
  "The meaning of ``Size`` for indefinite subtypes.  See
  13.3(48)."

Size for an indefinite subtype is the maximum possible size, except that
for the case of a subprogram parameter, the size of the parameter object
is the actual size.

*
  "The default external representation for a type tag.  See
  13.3(75)."

The default external representation for a type tag is the fully expanded
name of the type in upper case letters.

*
  "What determines whether a compilation unit is the same in
  two different partitions.  See 13.3(76)."

A compilation unit is the same in two different partitions if and only
if it derives from the same source file.

*
  "Implementation-defined components.  See 13.5.1(15)."

The only implementation defined component is the tag for a tagged type,
which contains a pointer to the dispatching table.

*
  "If ``Word_Size`` = ``Storage_Unit``, the default bit
  ordering.  See 13.5.3(5)."

``Word_Size`` (32) is not the same as ``Storage_Unit`` (8) for this
implementation, so no non-default bit ordering is supported.  The default
bit ordering corresponds to the natural endianness of the target architecture.

*
  "The contents of the visible part of package ``System``
  and its language-defined children.  See 13.7(2)."

See the definition of these packages in files :file:`system.ads` and
:file:`s-stoele.ads`. Note that two declarations are added to package
System.

.. code-block:: ada

  Max_Priority           : constant Positive := Priority'Last;
  Max_Interrupt_Priority : constant Positive := Interrupt_Priority'Last;

*
  "The contents of the visible part of package
  ``System.Machine_Code``, and the meaning of
  *code_statements*.  See 13.8(7)."

See the definition and documentation in file :file:`s-maccod.ads`.

*
  "The effect of unchecked conversion.  See 13.9(11)."

Unchecked conversion between types of the same size
results in an uninterpreted transmission of the bits from one type
to the other.  If the types are of unequal sizes, then in the case of
discrete types, a shorter source is first zero or sign extended as
necessary, and a shorter target is simply truncated on the left.
For all non-discrete types, the source is first copied if necessary
to ensure that the alignment requirements of the target are met, then
a pointer is constructed to the source value, and the result is obtained
by dereferencing this pointer after converting it to be a pointer to the
target type. Unchecked conversions where the target subtype is an
unconstrained array are not permitted. If the target alignment is
greater than the source alignment, then a copy of the result is
made with appropriate alignment

*
  "The semantics of operations on invalid representations.
  See 13.9.2(10-11)."

For assignments and other operations where the use of invalid values cannot
result in erroneous behavior, the compiler ignores the possibility of invalid
values. An exception is raised at the point where an invalid value would
result in erroneous behavior. For example executing:

.. code-block:: ada

  procedure invalidvals is
    X : Integer := -1;
    Y : Natural range 1 .. 10;
    for Y'Address use X'Address;
    Z : Natural range 1 .. 10;
    A : array (Natural range 1 .. 10) of Integer;
  begin
    Z := Y;     -- no exception
    A (Z) := 3; -- exception raised;
  end;

As indicated, an exception is raised on the array assignment, but not
on the simple assignment of the invalid negative value from Y to Z.

*
  "The manner of choosing a storage pool for an access type
  when ``Storage_Pool`` is not specified for the type.  See 13.11(17)."

There are 3 different standard pools used by the compiler when
``Storage_Pool`` is not specified depending whether the type is local
to a subprogram or defined at the library level and whether
``Storage_Size``is specified or not.  See documentation in the runtime
library units ``System.Pool_Global``, ``System.Pool_Size`` and
``System.Pool_Local`` in files :file:`s-poosiz.ads`,
:file:`s-pooglo.ads` and :file:`s-pooloc.ads` for full details on the
default pools used.

*
  "Whether or not the implementation provides user-accessible
  names for the standard pool type(s).  See 13.11(17)."

See documentation in the sources of the run time mentioned in the previous
paragraph.  All these pools are accessible by means of `with`\ ing
these units.

*
  "The meaning of ``Storage_Size``.  See 13.11(18)."

``Storage_Size`` is measured in storage units, and refers to the
total space available for an access type collection, or to the primary
stack space for a task.

*
  "Implementation-defined aspects of storage pools.  See
  13.11(22)."

See documentation in the sources of the run time mentioned in the
paragraph about standard storage pools above
for details on GNAT-defined aspects of storage pools.

*
  "The set of restrictions allowed in a pragma
  ``Restrictions``.  See 13.12(7)."

See :ref:`Standard_and_Implementation_Defined_Restrictions`.

*
  "The consequences of violating limitations on
  ``Restrictions`` pragmas.  See 13.12(9)."

Restrictions that can be checked at compile time result in illegalities
if violated.  Currently there are no other consequences of violating
restrictions.

*
  "The representation used by the ``Read`` and
  ``Write`` attributes of elementary types in terms of stream
  elements.  See 13.13.2(9)."

The representation is the in-memory representation of the base type of
the type, using the number of bits corresponding to the
``type'Size`` value, and the natural ordering of the machine.

*
  "The names and characteristics of the numeric subtypes
  declared in the visible part of package ``Standard``.  See A.1(3)."

See items describing the integer and floating-point types supported.

*
  "The string returned by ``Character_Set_Version``.
  See A.3.5(3)."

``Ada.Wide_Characters.Handling.Character_Set_Version`` returns
the string "Unicode 4.0", referring to version 4.0 of the
Unicode specification.

*
  "The accuracy actually achieved by the elementary
  functions.  See A.5.1(1)."

The elementary functions correspond to the functions available in the C
library.  Only fast math mode is implemented.

*
  "The sign of a zero result from some of the operators or
  functions in ``Numerics.Generic_Elementary_Functions``, when
  ``Float_Type'Signed_Zeros`` is ``True``.  See A.5.1(46)."

The sign of zeroes follows the requirements of the IEEE 754 standard on
floating-point.

*
  "The value of
  ``Numerics.Float_Random.Max_Image_Width``.  See A.5.2(27)."

Maximum image width is 6864, see library file :file:`s-rannum.ads`.

*
  "The value of
  ``Numerics.Discrete_Random.Max_Image_Width``.  See A.5.2(27)."

Maximum image width is 6864, see library file :file:`s-rannum.ads`.

*
  "The algorithms for random number generation.  See
  A.5.2(32)."

The algorithm is the Mersenne Twister, as documented in the source file
:file:`s-rannum.adb`. This version of the algorithm has a period of
2**19937-1.

*
  "The string representation of a random number generator's
  state.  See A.5.2(38)."

The value returned by the Image function is the concatenation of
the fixed-width decimal representations of the 624 32-bit integers
of the state vector.

*
  "The minimum time interval between calls to the
  time-dependent Reset procedure that are guaranteed to initiate different
  random number sequences.  See A.5.2(45)."

The minimum period between reset calls to guarantee distinct series of
random numbers is one microsecond.

*
  "The values of the ``Model_Mantissa``,
  ``Model_Emin``, ``Model_Epsilon``, ``Model``,
  ``Safe_First``, and ``Safe_Last`` attributes, if the Numerics
  Annex is not supported.  See A.5.3(72)."

Run the compiler with *-gnatS* to produce a listing of package
``Standard``, has the values of all numeric attributes.

*
  "Any implementation-defined characteristics of the
  input-output packages.  See A.7(14)."

There are no special implementation defined characteristics for these
packages.

*
  "The value of ``Buffer_Size`` in ``Storage_IO``.  See
  A.9(10)."

All type representations are contiguous, and the ``Buffer_Size`` is
the value of ``type'Size`` rounded up to the next storage unit
boundary.

*
  "External files for standard input, standard output, and
  standard error See A.10(5)."

These files are mapped onto the files provided by the C streams
libraries.  See source file :file:`i-cstrea.ads` for further details.

*
  "The accuracy of the value produced by ``Put``.  See
  A.10.9(36)."

If more digits are requested in the output than are represented by the
precision of the value, zeroes are output in the corresponding least
significant digit positions.

*
  "The meaning of ``Argument_Count``, ``Argument``, and
  ``Command_Name``.  See A.15(1)."

These are mapped onto the ``argv`` and ``argc`` parameters of the
main program in the natural manner.

*
  "The interpretation of the ``Form`` parameter in procedure
  ``Create_Directory``.  See A.16(56)."

The ``Form`` parameter is not used.

*
  "The interpretation of the ``Form`` parameter in procedure
  ``Create_Path``.  See A.16(60)."

The ``Form`` parameter is not used.

*
  "The interpretation of the ``Form`` parameter in procedure
  ``Copy_File``.  See A.16(68)."

The ``Form`` parameter is case-insensitive.
Two fields are recognized in the ``Form`` parameter::

  *preserve=<value>*
  *mode=<value>*

<value> starts immediately after the character '=' and ends with the
character immediately preceding the next comma (',') or with the last
character of the parameter.

The only possible values for preserve= are:

================== ===================================================================
Value              Meaning
================== ===================================================================
*no_attributes*    Do not try to preserve any file attributes. This is the
                   default if no preserve= is found in Form.
*all_attributes*   Try to preserve all file attributes (timestamps, access rights).
*timestamps*       Preserve the timestamp of the copied file, but not the other
                   file attributes.
================== ===================================================================

The only possible values for mode= are:

============== ===============================================================================
Value          Meaning
============== ===============================================================================
*copy*         Only do the copy if the destination file does not already exist.
               If it already exists, Copy_File fails.
*overwrite*    Copy the file in all cases. Overwrite an already existing destination file.
*append*       Append the original file to the destination file. If the destination file
               does not exist, the destination file is a copy of the source file.
               When mode=append, the field preserve=, if it exists, is not taken into account.
============== ===============================================================================

If the Form parameter includes one or both of the fields and the value or
values are incorrect, Copy_file fails with Use_Error.

Examples of correct Forms::

  Form => "preserve=no_attributes,mode=overwrite" (the default)
  Form => "mode=append"
  Form => "mode=copy, preserve=all_attributes"

Examples of incorrect Forms::

  Form => "preserve=junk"
  Form => "mode=internal, preserve=timestamps"

*
  "The interpretation of the ``Pattern`` parameter, when not the null string,
  in the ``Start_Search`` and ``Search`` procedures.
  See A.16(104) and A.16(112)."

When the ``Pattern`` parameter is not the null string, it is interpreted
according to the syntax of regular expressions as defined in the
``GNAT.Regexp`` package.

See :ref:`GNAT.Regexp_(g-regexp.ads)`.

*
  "Implementation-defined convention names.  See B.1(11)."

The following convention names are supported

======================= ==============================================================================
Convention Name         Interpretation
======================= ==============================================================================
*Ada*                   Ada
*Ada_Pass_By_Copy*      Allowed for any types except by-reference types such as limited
                        records. Compatible with convention Ada, but causes any parameters
                        with this convention to be passed by copy.
*Ada_Pass_By_Reference* Allowed for any types except by-copy types such as scalars.
                        Compatible with convention Ada, but causes any parameters
                        with this convention to be passed by reference.
*Assembler*             Assembly language
*Asm*                   Synonym for Assembler
*Assembly*              Synonym for Assembler
*C*                     C
*C_Pass_By_Copy*        Allowed only for record types, like C, but also notes that record
                        is to be passed by copy rather than reference.
*COBOL*                 COBOL
*C_Plus_Plus (or CPP)*  C++
*Default*               Treated the same as C
*External*              Treated the same as C
*Fortran*               Fortran
*Intrinsic*             For support of pragma ``Import`` with convention Intrinsic, see
                        separate section on Intrinsic Subprograms.
*Stdcall*               Stdcall (used for Windows implementations only).  This convention correspond
                        to the WINAPI (previously called Pascal convention) C/C++ convention under
                        Windows.  A routine with this convention cleans the stack before
                        exit. This pragma cannot be applied to a dispatching call.
*DLL*                   Synonym for Stdcall
*Win32*                 Synonym for Stdcall
*Stubbed*               Stubbed is a special convention used to indicate that the body of the
                        subprogram will be entirely ignored.  Any call to the subprogram
                        is converted into a raise of the ``Program_Error`` exception.  If a
                        pragma ``Import`` specifies convention ``stubbed`` then no body need
                        be present at all.  This convention is useful during development for the
                        inclusion of subprograms whose body has not yet been written.
                        In addition, all otherwise unrecognized convention names are also
                        treated as being synonymous with convention C.  In all implementations,
                        use of such other names results in a warning.
======================= ==============================================================================

*
  "The meaning of link names.  See B.1(36)."

Link names are the actual names used by the linker.

*
  "The manner of choosing link names when neither the link
  name nor the address of an imported or exported entity is specified.  See
  B.1(36)."

The default linker name is that which would be assigned by the relevant
external language, interpreting the Ada name as being in all lower case
letters.

*
  "The effect of pragma ``Linker_Options``.  See B.1(37)."

The string passed to ``Linker_Options`` is presented uninterpreted as
an argument to the link command, unless it contains ASCII.NUL characters.
NUL characters if they appear act as argument separators, so for example

.. code-block:: ada

  pragma Linker_Options ("-labc" & ASCII.NUL & "-ldef");

causes two separate arguments ``-labc`` and ``-ldef`` to be passed to the
linker. The order of linker options is preserved for a given unit. The final
list of options passed to the linker is in reverse order of the elaboration
order. For example, linker options for a body always appear before the options
from the corresponding package spec.

*
  "The contents of the visible part of package
  ``Interfaces`` and its language-defined descendants.  See B.2(1)."

See files with prefix :file:`i-` in the distributed library.

*
  "Implementation-defined children of package
  ``Interfaces``.  The contents of the visible part of package
  ``Interfaces``.  See B.2(11)."

See files with prefix :file:`i-` in the distributed library.

*
  "The types ``Floating``, ``Long_Floating``,
  ``Binary``, ``Long_Binary``, ``Decimal_ Element``, and
  ``COBOL_Character``; and the initialization of the variables
  ``Ada_To_COBOL`` and ``COBOL_To_Ada``, in
  ``Interfaces.COBOL``.  See B.4(50)."

===================== ====================================
COBOL                 Ada
===================== ====================================
*Floating*            Float
*Long_Floating*       (Floating) Long_Float
*Binary*              Integer
*Long_Binary*         Long_Long_Integer
*Decimal_Element*     Character
*COBOL_Character*     Character
===================== ====================================

For initialization, see the file :file:`i-cobol.ads` in the distributed library.

*
  "Support for access to machine instructions.  See C.1(1)."

See documentation in file :file:`s-maccod.ads` in the distributed library.

*
  "Implementation-defined aspects of access to machine
  operations.  See C.1(9)."

See documentation in file :file:`s-maccod.ads` in the distributed library.

*
  "Implementation-defined aspects of interrupts.  See C.3(2)."

Interrupts are mapped to signals or conditions as appropriate.  See
definition of unit
``Ada.Interrupt_Names`` in source file :file:`a-intnam.ads` for details
on the interrupts supported on a particular target.

*
  "Implementation-defined aspects of pre-elaboration.  See
  C.4(13)."

GNAT does not permit a partition to be restarted without reloading,
except under control of the debugger.

*
  "The semantics of pragma ``Discard_Names``.  See C.5(7)."

Pragma ``Discard_Names`` causes names of enumeration literals to
be suppressed.  In the presence of this pragma, the Image attribute
provides the image of the Pos of the literal, and Value accepts
Pos values.

For tagged types, when pragmas ``Discard_Names`` and ``No_Tagged_Streams``
simultaneously apply, their Expanded_Name and External_Tag are initialized
with empty strings. This is useful to avoid exposing entity names at binary
level.

*
  "The result of the ``Task_Identification.Image``
  attribute.  See C.7.1(7)."

The result of this attribute is a string that identifies
the object or component that denotes a given task. If a variable ``Var``
has a task type, the image for this task will have the form :samp:`Var_{XXXXXXXX}`,
where the suffix *XXXXXXXX*
is the hexadecimal representation of the virtual address of the corresponding
task control block. If the variable is an array of tasks, the image of each
task will have the form of an indexed component indicating the position of a
given task in the array, e.g., :samp:`Group(5)_{XXXXXXX}`. If the task is a
component of a record, the image of the task will have the form of a selected
component. These rules are fully recursive, so that the image of a task that
is a subcomponent of a composite object corresponds to the expression that
designates this task.

If a task is created by an allocator, its image depends on the context. If the
allocator is part of an object declaration, the rules described above are used
to construct its image, and this image is not affected by subsequent
assignments. If the allocator appears within an expression, the image
includes only the name of the task type.

If the configuration pragma Discard_Names is present, or if the restriction
No_Implicit_Heap_Allocation is in effect,  the image reduces to
the numeric suffix, that is to say the hexadecimal representation of the
virtual address of the control block of the task.

*
  "The value of ``Current_Task`` when in a protected entry
  or interrupt handler.  See C.7.1(17)."

Protected entries or interrupt handlers can be executed by any
convenient thread, so the value of ``Current_Task`` is undefined.

*
  "The effect of calling ``Current_Task`` from an entry
  body or interrupt handler.  See C.7.1(19)."

When GNAT can determine statically that ``Current_Task`` is called directly in
the body of an entry (or barrier) then a warning is emitted and ``Program_Error``
is raised at run time. Otherwise, the effect of calling ``Current_Task`` from an
entry body or interrupt handler is to return the identification of the task
currently executing the code.

*
  "Implementation-defined aspects of
  ``Task_Attributes``.  See C.7.2(19)."

There are no implementation-defined aspects of ``Task_Attributes``.

*
  "Values of all ``Metrics``.  See D(2)."

The metrics information for GNAT depends on the performance of the
underlying operating system.  The sources of the run-time for tasking
implementation, together with the output from *-gnatG* can be
used to determine the exact sequence of operating systems calls made
to implement various tasking constructs.  Together with appropriate
information on the performance of the underlying operating system,
on the exact target in use, this information can be used to determine
the required metrics.

*
  "The declarations of ``Any_Priority`` and
  ``Priority``.  See D.1(11)."

See declarations in file :file:`system.ads`.

*
  "Implementation-defined execution resources.  See D.1(15)."

There are no implementation-defined execution resources.

*
  "Whether, on a multiprocessor, a task that is waiting for
  access to a protected object keeps its processor busy.  See D.2.1(3)."

On a multi-processor, a task that is waiting for access to a protected
object does not keep its processor busy.

*
  "The affect of implementation defined execution resources
  on task dispatching.  See D.2.1(9)."

Tasks map to threads in the threads package used by GNAT.  Where possible
and appropriate, these threads correspond to native threads of the
underlying operating system.

*
  "Implementation-defined *policy_identifiers* allowed
  in a pragma ``Task_Dispatching_Policy``.  See D.2.2(3)."

There are no implementation-defined policy-identifiers allowed in this
pragma.

*
  "Implementation-defined aspects of priority inversion.  See
  D.2.2(16)."

Execution of a task cannot be preempted by the implementation processing
of delay expirations for lower priority tasks.

*
  "Implementation-defined task dispatching.  See D.2.2(18)."

The policy is the same as that of the underlying threads implementation.

*
  "Implementation-defined *policy_identifiers* allowed
  in a pragma ``Locking_Policy``.  See D.3(4)."

The two implementation defined policies permitted in GNAT are
``Inheritance_Locking`` and  ``Concurrent_Readers_Locking``. On
targets that support the ``Inheritance_Locking`` policy, locking is
implemented by inheritance, i.e., the task owning the lock operates
at a priority equal to the highest priority of any task currently
requesting the lock. On targets that support the
``Concurrent_Readers_Locking`` policy, locking is implemented with a
read/write lock allowing multiple protected object functions to enter
concurrently.

*
  "Default ceiling priorities.  See D.3(10)."

The ceiling priority of protected objects of the type
``System.Interrupt_Priority'Last`` as described in the Ada
Reference Manual D.3(10),

*
  "The ceiling of any protected object used internally by
  the implementation.  See D.3(16)."

The ceiling priority of internal protected objects is
``System.Priority'Last``.

*
  "Implementation-defined queuing policies.  See D.4(1)."

There are no implementation-defined queuing policies.

*
  "On a multiprocessor, any conditions that cause the
  completion of an aborted construct to be delayed later than what is
  specified for a single processor.  See D.6(3)."

The semantics for abort on a multi-processor is the same as on a single
processor, there are no further delays.

*
  "Any operations that implicitly require heap storage
  allocation.  See D.7(8)."

The only operation that implicitly requires heap storage allocation is
task creation.

*
  "What happens when a task terminates in the presence of
  pragma ``No_Task_Termination``. See D.7(15)."

Execution is erroneous in that case.

*
  "Implementation-defined aspects of pragma
  ``Restrictions``.  See D.7(20)."

There are no such implementation-defined aspects.

*
  "Implementation-defined aspects of package
  ``Real_Time``.  See D.8(17)."

There are no implementation defined aspects of package ``Real_Time``.

*
  "Implementation-defined aspects of
  *delay_statements*.  See D.9(8)."

Any difference greater than one microsecond will cause the task to be
delayed (see D.9(7)).

*
  "The upper bound on the duration of interrupt blocking
  caused by the implementation.  See D.12(5)."

The upper bound is determined by the underlying operating system.  In
no cases is it more than 10 milliseconds.

*
  "The means for creating and executing distributed
  programs.  See E(5)."

The GLADE package provides a utility GNATDIST for creating and executing
distributed programs.  See the GLADE reference manual for further details.

*
  "Any events that can result in a partition becoming
  inaccessible.  See E.1(7)."

See the GLADE reference manual for full details on such events.

*
  "The scheduling policies, treatment of priorities, and
  management of shared resources between partitions in certain cases.  See
  E.1(11)."

See the GLADE reference manual for full details on these aspects of
multi-partition execution.

*
  "Events that cause the version of a compilation unit to
  change.  See E.3(5)."

Editing the source file of a compilation unit, or the source files of
any units on which it is dependent in a significant way cause the version
to change.  No other actions cause the version number to change.  All changes
are significant except those which affect only layout, capitalization or
comments.

*
  "Whether the execution of the remote subprogram is
  immediately aborted as a result of cancellation.  See E.4(13)."

See the GLADE reference manual for details on the effect of abort in
a distributed application.

*
  "Implementation-defined aspects of the PCS.  See E.5(25)."

See the GLADE reference manual for a full description of all implementation
defined aspects of the PCS.

*
  "Implementation-defined interfaces in the PCS.  See
  E.5(26)."

See the GLADE reference manual for a full description of all
implementation defined interfaces.

*
  "The values of named numbers in the package
  ``Decimal``.  See F.2(7)."

==================== ==========
Named Number         Value
==================== ==========
*Max_Scale*           +18
*Min_Scale*           -18
*Min_Delta*           1.0E-18
*Max_Delta*           1.0E+18
*Max_Decimal_Digits*  18
==================== ==========

*
  "The value of ``Max_Picture_Length`` in the package
  ``Text_IO.Editing``.  See F.3.3(16)."

64

*
  "The value of ``Max_Picture_Length`` in the package
  ``Wide_Text_IO.Editing``.  See F.3.4(5)."

64

*
  "The accuracy actually achieved by the complex elementary
  functions and by other complex arithmetic operations.  See G.1(1)."

Standard library functions are used for the complex arithmetic
operations.  Only fast math mode is currently supported.

*
  "The sign of a zero result (or a component thereof) from
  any operator or function in ``Numerics.Generic_Complex_Types``, when
  ``Real'Signed_Zeros`` is True.  See G.1.1(53)."

The signs of zero values are as recommended by the relevant
implementation advice.

*
  "The sign of a zero result (or a component thereof) from
  any operator or function in
  ``Numerics.Generic_Complex_Elementary_Functions``, when
  ``Real'Signed_Zeros`` is ``True``.  See G.1.2(45)."

The signs of zero values are as recommended by the relevant
implementation advice.

*
  "Whether the strict mode or the relaxed mode is the
  default.  See G.2(2)."

The strict mode is the default.  There is no separate relaxed mode.  GNAT
provides a highly efficient implementation of strict mode.

*
  "The result interval in certain cases of fixed-to-float
  conversion.  See G.2.1(10)."

For cases where the result interval is implementation dependent, the
accuracy is that provided by performing all operations in 64-bit IEEE
floating-point format.

*
  "The result of a floating point arithmetic operation in
  overflow situations, when the ``Machine_Overflows`` attribute of the
  result type is ``False``.  See G.2.1(13)."

Infinite and NaN values are produced as dictated by the IEEE
floating-point standard.
Note that on machines that are not fully compliant with the IEEE
floating-point standard, such as Alpha, the *-mieee* compiler flag
must be used for achieving IEEE conforming behavior (although at the cost
of a significant performance penalty), so infinite and NaN values are
properly generated.

*
  "The result interval for division (or exponentiation by a
  negative exponent), when the floating point hardware implements division
  as multiplication by a reciprocal.  See G.2.1(16)."

Not relevant, division is IEEE exact.

*
  "The definition of close result set, which determines the
  accuracy of certain fixed point multiplications and divisions.  See
  G.2.3(5)."

Operations in the close result set are performed using IEEE long format
floating-point arithmetic.  The input operands are converted to
floating-point, the operation is done in floating-point, and the result
is converted to the target type.

*
  "Conditions on a *universal_real* operand of a fixed
  point multiplication or division for which the result shall be in the
  perfect result set.  See G.2.3(22)."

The result is only defined to be in the perfect result set if the result
can be computed by a single scaling operation involving a scale factor
representable in 64-bits.

*
  "The result of a fixed point arithmetic operation in
  overflow situations, when the ``Machine_Overflows`` attribute of the
  result type is ``False``.  See G.2.3(27)."

Not relevant, ``Machine_Overflows`` is ``True`` for fixed-point
types.

*
  "The result of an elementary function reference in
  overflow situations, when the ``Machine_Overflows`` attribute of the
  result type is ``False``.  See G.2.4(4)."

IEEE infinite and Nan values are produced as appropriate.

*
  "The value of the angle threshold, within which certain
  elementary functions, complex arithmetic operations, and complex
  elementary functions yield results conforming to a maximum relative
  error bound.  See G.2.4(10)."

Information on this subject is not yet available.

*
  "The accuracy of certain elementary functions for
  parameters beyond the angle threshold.  See G.2.4(10)."

Information on this subject is not yet available.

*
  "The result of a complex arithmetic operation or complex
  elementary function reference in overflow situations, when the
  ``Machine_Overflows`` attribute of the corresponding real type is
  ``False``.  See G.2.6(5)."

IEEE infinite and Nan values are produced as appropriate.

*
  "The accuracy of certain complex arithmetic operations and
  certain complex elementary functions for parameters (or components
  thereof) beyond the angle threshold.  See G.2.6(8)."

Information on those subjects is not yet available.

*
  "Information regarding bounded errors and erroneous
  execution.  See H.2(1)."

Information on this subject is not yet available.

*
  "Implementation-defined aspects of pragma
  ``Inspection_Point``.  See H.3.2(8)."

Pragma ``Inspection_Point`` ensures that the variable is live and can
be examined by the debugger at the inspection point.

*
  "Implementation-defined aspects of pragma
  ``Restrictions``.  See H.4(25)."

There are no implementation-defined aspects of pragma ``Restrictions``.  The
use of pragma ``Restrictions [No_Exceptions]`` has no effect on the
generated code.  Checks must suppressed by use of pragma ``Suppress``.

*
  "Any restrictions on pragma ``Restrictions``.  See
  H.4(27)."

There are no restrictions on pragma ``Restrictions``.
