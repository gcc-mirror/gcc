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
followed by a description of how GNAT handles the implementation dependence.

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
  "The semantics of an Ada program whose text is not in
  Normalization Form C.  See 2.1(4)."

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
  "The message string associated with the Assertion_Error exception raised
  by the failure of a predicate check if there is no applicable
  Predicate_Failure aspect.  See 3.2.4(31)."

In the case of a Dynamic_Predicate aspect, the string is
"Dynamic_Predicate failed at <source position>", where
"<source position>" might be something like "foo.adb:123".
The Static_Predicate case is handled analogously.

*
  "The predefined integer types declared in
  ``Standard``.  See 3.5.4(25)."

.. tabularcolumns:: |l|L|

========================= =======================================
Type                       Representation
========================= =======================================
*Short_Short_Integer*      8-bit signed
*Short_Integer*            16-bit signed
*Integer*                  32-bit signed
*Long_Integer*             64-bit signed (on most 64-bit targets,
                           depending on the C definition of long)
                           32-bit signed (on all other targets)
*Long_Long_Integer*        64-bit signed
*Long_Long_Long_Integer*   128-bit signed (on 64-bit targets)
                           64-bit signed (on 32-bit targets)
========================= =======================================

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

The precision and range are defined by the IEEE Standard for Floating-Point
Arithmetic (IEEE 754-2019).

*
  "The predefined floating point types declared in
  ``Standard``.  See 3.5.7(16)."

====================== ===============================================
Type                   Representation
====================== ===============================================
*Short_Float*          IEEE Binary32 (Single)
*Float*                IEEE Binary32 (Single)
*Long_Float*           IEEE Binary64 (Double)
*Long_Long_Float*      IEEE Binary64 (Double) on non-x86 architectures
                       IEEE 80-bit Extended on x86 architecture
====================== ===============================================

The default rounding mode specified by the IEEE 754 Standard is assumed both
for static and dynamic computations (that is, round to nearest, ties to even).
The input routines yield correctly rounded values for Short_Float, Float, and
Long_Float at least. The output routines can compute up to twice as many exact
digits as the value of ``T'Digits`` for any type, for example 30 digits for
Long_Float; if more digits are requested, zeros are printed.

*
  "The small of an ordinary fixed point type.  See 3.5.9(8)."

The small is the largest power of two that does not exceed the delta.

*
  "What combinations of small, range, and digits are
  supported for fixed point types.  See 3.5.9(10)."

For an ordinary fixed point type, on 32-bit platforms, the small must lie in
2.0**(-80) .. 2.0**80 and the range in -9.0E+36 .. 9.0E+36; any combination
is permitted that does not result in a mantissa larger than 63 bits.

On 64-bit platforms, the small must lie in 2.0**(-127) .. 2.0**127 and the
range in -1.0E+76 .. 1.0E+76; any combination is permitted that does not
result in a mantissa larger than 63 bits, and any combination is permitted
that results in a mantissa between 64 and 127 bits if the small is the
ratio of two integers that lie in 1 .. 2.0**127.

If the small is the ratio of two integers with 64-bit magnitude on 32-bit
platforms and 128-bit magnitude on 64-bit platforms, which is the case if
no ``small`` clause is provided, then the operations of the fixed point
type are entirely implemented by means of integer instructions.  In the
other cases, some operations, in particular input and output, may be
implemented by means of floating-point instructions and may be affected
by accuracy issues on architectures other than x86.

For a decimal fixed point type, on 32-bit platforms, the small must lie in
1.0E-18 .. 1.0E+18 and the digits in 1 .. 18.  On 64-bit platforms, the
small must lie in 1.0E-38 .. 1.0E+38 and the digits in 1 .. 38.

*
  "The result of ``Tags.Expanded_Name`` for types declared
  within an unnamed *block_statement*.  See 3.9(10)."

Block numbers of the form :samp:`B{nnn}`, where *nnn* is a
decimal integer are allocated.

*
  "The sequence of characters of the value returned by Tags.Expanded_Name
  (respectively, Tags.Wide_Expanded_Name) when some of the graphic
  characters of Tags.Wide_Wide_Expanded_Name are not defined in Character
  (respectively, Wide_Character).  See 3.9(10.1)."

This is handled in the same way as the implementation-defined behavior
referenced in A.4.12(34).

*
  "Implementation-defined attributes.  See 4.1.4(12)."

See :ref:`Implementation_Defined_Attributes`.

*
  "The value of the parameter to Empty for some container aggregates.
  See 4.3.5(40)."

As per the suggestion given in the Annotated Ada RM, the default value
of the formal parameter is used if one exists and zero is used otherwise.

*
  "The maximum number of chunks for a parallel reduction expression without
  a chunk_specification.  See 4.5.10(21)."

Feature unimplemented.

*
  "Rounding of real static expressions which are exactly half-way between
  two machine numbers.  See 4.9(38)."

Round to even is used in all such cases.

*
  "The maximum number of chunks for a parallel generalized iterator without
  a chunk_specification.  See 5.5.2(10)."

Feature unimplemented.

*
  "The number of chunks for an array component iterator.  See 5.5.2(11)."

Feature unimplemented.

*
  "Any extensions of the Global aspect.  See 6.1.2(43)."

Feature unimplemented.

*
  "The circumstances the implementation passes in the null value for a view
  conversion of an access type used as an out parameter.  See 6.4.1(19)."

Difficult to characterize.

*
  "Any extensions of the Default_Initial_Condition aspect.  See 7.3.3(11)."

SPARK allows specifying *null* as the Default_Initial_Condition
aspect of a type. See the SPARK reference manual for further details.

*
  "Any implementation-defined time types.  See 9.6(6)."

There are no implementation-defined time types.

*
  "The time base associated with relative delays.  See 9.6(20)."

See 9.6(20).  The time base used is that provided by the C library
function ``gettimeofday``.

*
  "The time base of the type ``Calendar.Time``.  See 9.6(23)."

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
  "The result of Calendar.Formatting.Image if its argument represents more
  than 100 hours.  See 9.6.1(86)."

Calendar.Time_Error is raised.

*
  "Implementation-defined conflict check policies.  See 9.10.1(5)."

There are no implementation-defined conflict check policies.

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
  "The implementation-defined means, if any, of specifying which compilation
  units are needed by a given compilation unit.  See 10.2(2)."

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
  "The order of elaboration of *library_items*.  See 10.2(18)."

The first constraint on ordering is that it meets the requirements of
Chapter 10 of the Ada Reference Manual.  This still leaves some
implementation-dependent choices, which are resolved by analyzing
the elaboration code of each unit and identifying implicit
elaboration-order dependencies.

*
  "Parameter passing and function return for the main
  subprogram.  See 10.2(21)."

The main program has no parameters.  It may be a procedure, or a function
returning an integer type.  In the latter case, the returned integer
value is the return code of the program (overriding any value that
may have been set by a call to ``Ada.Command_Line.Set_Exit_Status``).

*
  "The mechanisms for building and running partitions.  See 10.2(24)."

GNAT itself supports programs with only a single partition. The GNATDIST
tool provided with the GLADE package (which also includes an implementation
of the PCS) provides a completely flexible method for building and running
programs consisting of multiple partitions. See the separate GLADE manual
for details.

*
  "The details of program execution, including program
  termination.  See 10.2(25)."

See separate section on compilation model.

*
  "The semantics of any non-active partitions supported by the
  implementation.  See 10.2(28)."

Passive partitions are supported on targets where shared memory is
provided by the operating system. See the GLADE reference manual for
further details.

*
  "The information returned by ``Exception_Message``.  See 11.4.1(10)."

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
  "The sequence of characters of the value returned by
  Exceptions.Exception_Name (respectively, Exceptions.Wide_Exception_Name)
  when some of the graphic characters of Exceptions.Wide_Wide_Exception_Name
  are not defined in Character (respectively, Wide_Character).
  See 11.4.1(12.1)."

This is handled in the same way as the implementation-defined behavior
referenced in A.4.12(34).

*
  "The information returned by Exception_Information.  See 11.4.1(13)."

The exception name and the source location at which the exception was
raised are included.

*
  "Implementation-defined policy_identifiers and assertion_aspect_marks
  allowed in a pragma Assertion_Policy.  See 11.4.2(9)."

Implementation-defined assertion_aspect_marks include Assert_And_Cut,
Assume, Contract_Cases, Debug, Ghost, Initial_Condition, Loop_Invariant,
Loop_Variant, Postcondition, Precondition, Predicate, Refined_Post,
Statement_Assertions, and Subprogram_Variant. Implementation-defined
policy_identifiers include Ignore and Suppressible.

*
  "The default assertion policy.  See 11.4.2(10)."

The default assertion policy is Ignore, although this can be overridden
via compiler switches such as "-gnata".

*
  "Implementation-defined check names.  See 11.5(27)."

The implementation-defined check names include Alignment_Check,
Container_Checks, Duplicated_Tag_Check, Predicate_Check,
Raise_Check, Tampering_Check, and Validity_Check. In addition, a
user program can add implementation-defined check names by means
of the pragma Check_Name. See the description of pragma
``Suppress`` for details.

*
  "Existence and meaning of second parameter of pragma Unsuppress.
  See 11.5(27.1)."

The legality rules for and semantics of the second parameter of pragma
Unsuppress match those for the second argument of pragma Suppress.

*
  "The cases that cause conflicts between the representation of the
  ancestors of a type_declaration.  See 13.1(13.1)."

No such cases exist.

*
  "The interpretation of each representation aspect.  See 13.1(20)."

See separate section on data representations.

*
  "Any restrictions placed upon the specification of representation aspects.
  See 13.1(20)."

See separate section on data representations.

*
  "Implementation-defined aspects, including the syntax for specifying
  such aspects and the legality rules for such aspects.  See 13.1.1(38)."

See :ref:`Implementation_Defined_Aspects`.

*
  "The set of machine scalars.  See 13.3(8.1)."

See separate section on data representations.

*
  "The meaning of ``Size`` for indefinite subtypes.  See 13.3(48)."

The Size attribute of an indefinite subtype is not less than the Size
attribute of any object of that type.

*
  "The meaning of Object_Size for indefinite subtypes.  See 13.3(58)."

The Object_Size attribute of an indefinite subtype is not less than the
Object_Size attribute of any object of that type.

*
  "The default external representation for a type tag.  See 13.3(75)."

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

``Word_Size`` does not equal ``Storage_Unit`` in this implementation.

*
  "The contents of the visible part of package ``System``.  See 13.7(2)."

See the definition of package System in :file:`system.ads`.
Note that two declarations are added to package System.

.. code-block:: ada

  Max_Priority           : constant Positive := Priority'Last;
  Max_Interrupt_Priority : constant Positive := Interrupt_Priority'Last;

*
  "The range of Storage_Elements.Storage_Offset, the modulus of
  Storage_Elements.Storage_Element, and the declaration of
  Storage_Elements.Integer_Address.  See 13.7.1(11)."

See the definition of package System.Storage_Elements in :file:`s-stoele.ads`.

*
  "The contents of the visible part of package ``System.Machine_Code``,
  and the meaning of *code_statements*.  See 13.8(7)."

See the definition and documentation in file :file:`s-maccod.ads`.

*
  "The result of unchecked conversion for instances with scalar result
  types whose result is not defined by the language.  See 13.9(11)."

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
  "The result of unchecked conversion for instances with nonscalar result
  types whose result is not defined by the language.  See 13.9(11)."

See preceding definition for the scalar result case.

*
  "Whether or not the implementation provides user-accessible
  names for the standard pool type(s).  See 13.11(17)."

There are 3 different standard pools used by the compiler when
``Storage_Pool`` is not specified depending whether the type is local
to a subprogram or defined at the library level and whether
``Storage_Size``is specified or not. See documentation in the runtime
library units ``System.Pool_Global``, ``System.Pool_Size`` and
``System.Pool_Local`` in files :file:`s-poosiz.ads`,
:file:`s-pooglo.ads` and :file:`s-pooloc.ads` for full details on the
default pools used.  All these pools are accessible by means of `with`\ ing
these units.

*
  "The meaning of ``Storage_Size`` when neither the Storage_Size nor the
  Storage_Pool is specified for an access type.  See 13.11(18)."

``Storage_Size`` is measured in storage units, and refers to the
total space available for an access type collection, or to the primary
stack space for a task.

*
  "The effect of specifying aspect Default_Storage_Pool on an instance
  of a language-defined generic unit.  See 13.11.3(5)."

Instances of language-defined generic units are treated the same as other
instances with respect to the Default_Storage_Pool aspect.

*
  "Implementation-defined restrictions allowed in a pragma
  ``Restrictions``.  See 13.12(8.7)."

See :ref:`Standard_and_Implementation_Defined_Restrictions`.

*
  "The consequences of violating limitations on
  ``Restrictions`` pragmas.  See 13.12(9)."

Restrictions that can be checked at compile time are enforced at
compile time; violations are illegal. For other restrictions, any
violation during program execution results in erroneous execution.

*
  "Implementation-defined usage profiles allowed in a pragma Profile.
  See 13.12(15)."

See :ref:`Implementation_Defined_Pragmas`.

*
  "The contents of the stream elements read and written by the Read and
  Write attributes of elementary types.  See 13.13.2(9)."

The representation is the in-memory representation of the base type of
the type, using the number of bits corresponding to the
``type'Size`` value, and the natural ordering of the machine.

*
  "The names and characteristics of the numeric subtypes
  declared in the visible part of package ``Standard``.  See A.1(3)."

See items describing the integer and floating-point types supported.

*
  "The values returned by Strings.Hash.  See A.4.9(3)."

This hash function has predictable collisions and is subject to
equivalent substring attacks. It is not suitable for construction of a
hash table keyed on possibly malicious user input.

*
  "The value returned by a call to a Text_Buffer Get procedure if any
  character in the returned sequence is not defined in Character.
  See A.4.12(34)."

The contents of a buffer is represented internally as a UTF_8 string.
The value return by Text_Buffer.Get is the result of passing that
UTF_8 string to UTF_Encoding.Strings.Decode.

*
  "The value returned by a call to a Text_Buffer Wide_Get procedure if
  any character in the returned sequence is not defined in Wide_Character.
  See A.4.12(34)."

The contents of a buffer is represented internally as a UTF_8 string.
The value return by Text_Buffer.Wide_Get is the result of passing that
UTF_8 string to UTF_Encoding.Wide_Strings.Decode.

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
  "The string representation of a random number generator's
  state.  See A.5.2(38)."

The value returned by the Image function is the concatenation of
the fixed-width decimal representations of the 624 32-bit integers
of the state vector.

*
  "The values of the ``Model_Mantissa``,
  ``Model_Emin``, ``Model_Epsilon``, ``Model``,
  ``Safe_First``, and ``Safe_Last`` attributes, if the Numerics
  Annex is not supported.  See A.5.3(72)."

Running the compiler with *-gnatS* to produce a listing of package
``Standard`` displays the values of these attributes.

*
  "The value of ``Buffer_Size`` in ``Storage_IO``.  See A.9(10)."

All type representations are contiguous, and the ``Buffer_Size`` is
the value of ``type'Size`` rounded up to the next storage unit
boundary.

*
  "External files for standard input, standard output, and
  standard error See A.10(5)."

These files are mapped onto the files provided by the C streams
libraries. See source file :file:`i-cstrea.ads` for further details.

*
  "The accuracy of the value produced by ``Put``.  See A.10.9(36)."

If more digits are requested in the output than are represented by the
precision of the value, zeroes are output in the corresponding least
significant digit positions.

*
  "Current size for a stream file for which positioning is not supported.
  See A.12.1(1.1)."

Positioning is supported.

*
  "The meaning of ``Argument_Count``, ``Argument``, and
  ``Command_Name``.  See A.15(1)."

These are mapped onto the ``argv`` and ``argc`` parameters of the
main program in the natural manner.

*
  "The interpretation of file names and directory names.  See A.16(46)."

These names are interpreted consistently with the underlying file system.

*
  "The maxium value for a file size in Directories.  See A.16(87)."

Directories.File_Size'Last is equal to Long_Long_Integer'Last .

*
  "The result for Directories.Size for a directory or special file.
  See A.16(93)."

Name_Error is raised.

*
  "The result for Directories.Modification_Time for a directory or special file.
  See A.16(93)."

Name_Error is raised.

*
  "The interpretation of a nonnull search pattern in Directories.
  See A.16(104)."

When the ``Pattern`` parameter is not the null string, it is interpreted
according to the syntax of regular expressions as defined in the
``GNAT.Regexp`` package.

See :ref:`GNAT.Regexp_(g-regexp.ads)`.

*
  "The results of a Directories search if the contents of the directory are
  altered while a search is in progress.  See A.16(110)."

The effect of a call to Get_Next_Entry is determined by the current
state of the directory.

*
  "The definition and meaning of an environment variable.  See A.17(1)."

This definition is determined by the underlying operating system.

*
  "The circumstances where an environment variable cannot be defined.
  See A.17(16)."

There are no such implementation-defined circumstances.

*
  "Environment names for which Set has the effect of Clear.  See A.17(17)."

There are no such names.

*
  "The value of Containers.Hash_Type'Modulus. The value of
  Containers.Count_Type'Last.  See A.18.1(7)."

Containers.Hash_Type'Modulus is 2**32.
Containers.Count_Type'Last is 2**31 - 1.

*
  "Implementation-defined convention names.  See B.1(11)."

The following convention names are supported

.. tabularcolumns:: |l|L|

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
  "The manner of choosing link names when neither the link name nor the
  address of an imported or exported entity is specified.  See B.1(36)."

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
  "The definitions of certain types and constants in Interfaces.C.
  See B.3(41)."

See source file :file:`i-c.ads`.

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
  "The types Fortran_Integer, Real, Double_Precision, and Character_Set
  in Interfaces.Fortran.  See B.5(17)."

See source file :file:`i-fortra.ads`. These types are derived, respectively,
from Integer, Float, Long_Float, and Character.

*
  "Implementation-defined intrinsic subprograms.  See C.1(1)."

See separate section on Intrinsic Subprograms.

*
  "Any restrictions on a protected procedure or its containing type when an
  aspect Attach_handler or Interrupt_Handler is specified.  See C.3.1(17)."

There are no such restrictions.

*
  "Any other forms of interrupt handler supported by the Attach_Handler and
  Interrupt_Handler aspects.  See C.3.1(19)."

There are no such forms.

*
  "The semantics of some attributes and functions of an entity for which
  aspect Discard_Names is True.  See C.5(7)."

If Discard_Names is True for an enumeration type, the Image attribute
provides the image of the Pos of the literal, and Value accepts
Pos values.

If both of the aspects``Discard_Names`` and ``No_Tagged_Streams`` are true
for a tagged type, its Expanded_Name and External_Tag values are
empty strings. This is useful to avoid exposing entity names at binary
level.

*
  "The modulus and size of Test_and_Set_Flag.  See C.6.3(8)."

The modulus is 2**8. The size is 8.

*
  "The value used to represent the set value for Atomic_Test_and_Set.
  See C.6.3(10)."

The value is 1.

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
  "Granularity of locking for Task_Attributes.  See C.7.2(16)."

No locking is needed if the formal type Attribute has the size and
alignment of either Integer or System.Address and the bit representation
of Initial_Value is all zeroes. Otherwise, locking is performed.

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
  "Implementation-defined task dispatching policies.  See D.2.2(3)."

There are no implementation-defined task dispatching policies.

*
  "The value of Default_Quantum in Dispatching.Round_Robin.  See D.2.5(4)."

The value is 10 milliseconds.

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
  "Implementation-defined admission policies.  See D.4.1(1)."

There are no implementation-defined admission policies.

*
  "Any operations that implicitly require heap storage
  allocation.  See D.7(8)."

The only operation that implicitly requires heap storage allocation is
task creation.

*
  "When restriction No_Dynamic_CPU_Assignment applies to a partition, the
  processor on which a task with a CPU value of a Not_A_Specific_CPU will
  execute.  See D.7(10)."

Unknown.

*
  "When restriction No_Task_Termination applies to a partition, what happens
  when a task terminates.  See D.7(15.1)."

Execution is erroneous in that case.

*
  "The behavior when restriction Max_Storage_At_Blocking is violated.
  See D.7(17)."

Execution is erroneous in that case.

*
  "The behavior when restriction Max_Asynchronous_Select_Nesting is violated.
  See D.7(18)."

Execution is erroneous in that case.

*
  "The behavior when restriction Max_Tasks is violated.  See D.7(19)."

Execution is erroneous in that case.

* "Whether the use of pragma Restrictions results in a reduction in program
  code or data size or execution time.  See D.7(20)."

Yes it can, but the precise circumstances and properties of such reductions
are difficult to characterize.

*
  "The value of Barrier_Limit'Last in Synchronous_Barriers.  See D.10.1(4)."

Synchronous_Barriers.Barrier_Limit'Last is Integer'Last .

*
  "When an aborted task that is waiting on a Synchronous_Barrier is aborted.
  See D.10.1(13)."

Difficult to characterize.

*
  "The value of Min_Handler_Ceiling in Execution_Time.Group_Budgets.
  See D.14.2(7)."

See source file :file:`a-etgrbu.ads`.

*
  "The value of CPU_Range'Last in System.Multiprocessors.  See D.16(4)."

See source file :file:`s-multip.ads`.

*
  "The processor on which the environment task executes in the absence
  of a value for the aspect CPU.  See D.16(13)."

Unknown.

*
  "The means for creating and executing distributed
  programs.  See E(5)."

The GLADE package provides a utility GNATDIST for creating and executing
distributed programs. See the GLADE reference manual for further details.

*
  "Any events that can result in a partition becoming
  inaccessible.  See E.1(7)."

See the GLADE reference manual for full details on such events.

*
  "The scheduling policies, treatment of priorities, and management of
  shared resources between partitions in certain cases.  See E.1(11)."

See the GLADE reference manual for full details on these aspects of
multi-partition execution.

*
  "Whether the execution of the remote subprogram is
  immediately aborted as a result of cancellation.  See E.4(13)."

See the GLADE reference manual for details on the effect of abort in
a distributed application.

*
  "The range of type System.RPC.Partition_Id.  See E.5(14)."

System.RPC.Partition_ID'Last is Integer'Last. See source file :file:`s-rpc.ads`.

*
  "Implementation-defined interfaces in the PCS.  See E.5(26)."

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
  "The definition of close result set, which determines the accuracy of
  certain fixed point multiplications and divisions.  See G.2.3(5)."

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
representable in 64 bits.

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
  "The accuracy requirements for the subprograms Solve, Inverse,
  Determinant, Eigenvalues and Eigensystem for type Real_Matrix.
  See G.3.1(81)."

Information on those subjects is not yet available.

*
  "The accuracy requirements for the subprograms Solve, Inverse,
  Determinant, Eigenvalues and Eigensystem for type Complex_Matrix.
  See G.3.2(149)."

Information on those subjects is not yet available.

*
  "The consequences of violating No_Hidden_Indirect_Globals.  See H.4(23.9)."

Execution is erroneous in that case.
