.. _Standard_Library_Routines:

*************************
Standard Library Routines
*************************

The Ada Reference Manual contains in Annex A a full description of an
extensive set of standard library routines that can be used in any Ada
program, and which must be provided by all Ada compilers.  They are
analogous to the standard C library used by C programs.

GNAT implements all of the facilities described in annex A, and for most
purposes the description in the Ada Reference Manual, or appropriate Ada
text book, will be sufficient for making use of these facilities.

In the case of the input-output facilities,
:ref:`The_Implementation_of_Standard_I/O`,
gives details on exactly how GNAT interfaces to the
file system.  For the remaining packages, the Ada Reference Manual
should be sufficient.  The following is a list of the packages included,
together with a brief description of the functionality that is provided.

For completeness, references are included to other predefined library
routines defined in other sections of the Ada Reference Manual (these are
cross-indexed from Annex A). For further details see the relevant
package declarations in the run-time library. In particular, a few units
are not implemented, as marked by the presence of pragma Unimplemented_Unit,
and in this case the package declaration contains comments explaining why
the unit is not implemented.



``Ada`` *(A.2)*
  This is a parent package for all the standard library packages.  It is
  usually included implicitly in your program, and itself contains no
  useful data or routines.


``Ada.Assertions`` *(11.4.2)*
  ``Assertions`` provides the ``Assert`` subprograms, and also
  the declaration of the ``Assertion_Error`` exception.


``Ada.Asynchronous_Task_Control`` *(D.11)*
  ``Asynchronous_Task_Control`` provides low level facilities for task
  synchronization. It is typically not implemented. See package spec for details.


``Ada.Calendar`` *(9.6)*
  ``Calendar`` provides time of day access, and routines for
  manipulating times and durations.


``Ada.Calendar.Arithmetic`` *(9.6.1)*
  This package provides additional arithmetic
  operations for ``Calendar``.


``Ada.Calendar.Formatting`` *(9.6.1)*
  This package provides formatting operations for ``Calendar``.


``Ada.Calendar.Time_Zones`` *(9.6.1)*
  This package provides additional ``Calendar`` facilities
  for handling time zones.


``Ada.Characters`` *(A.3.1)*
  This is a dummy parent package that contains no useful entities


``Ada.Characters.Conversions`` *(A.3.2)*
  This package provides character conversion functions.


``Ada.Characters.Handling`` *(A.3.2)*
  This package provides some basic character handling capabilities,
  including classification functions for classes of characters (e.g., test
  for letters, or digits).


``Ada.Characters.Latin_1`` *(A.3.3)*
  This package includes a complete set of definitions of the characters
  that appear in type CHARACTER.  It is useful for writing programs that
  will run in international environments.  For example, if you want an
  upper case E with an acute accent in a string, it is often better to use
  the definition of ``UC_E_Acute`` in this package.  Then your program
  will print in an understandable manner even if your environment does not
  support these extended characters.


``Ada.Command_Line`` *(A.15)*
  This package provides access to the command line parameters and the name
  of the current program (analogous to the use of ``argc`` and ``argv``
  in C), and also allows the exit status for the program to be set in a
  system-independent manner.


``Ada.Complex_Text_IO`` *(G.1.3)*
  This package provides text input and output of complex numbers.


``Ada.Containers`` *(A.18.1)*
  A top level package providing a few basic definitions used by all the
  following specific child packages that provide specific kinds of
  containers.

``Ada.Containers.Bounded_Priority_Queues`` *(A.18.31)*

``Ada.Containers.Bounded_Synchronized_Queues`` *(A.18.29)*

``Ada.Containers.Doubly_Linked_Lists`` *(A.18.3)*

``Ada.Containers.Generic_Array_Sort`` *(A.18.26)*

``Ada.Containers.Generic_Constrained_Array_Sort`` *(A.18.26)*

``Ada.Containers.Generic_Sort`` *(A.18.26)*

``Ada.Containers.Hashed_Maps`` *(A.18.5)*

``Ada.Containers.Hashed_Sets`` *(A.18.8)*

``Ada.Containers.Indefinite_Doubly_Linked_Lists`` *(A.18.12)*

``Ada.Containers.Indefinite_Hashed_Maps`` *(A.18.13)*

``Ada.Containers.Indefinite_Hashed_Sets`` *(A.18.15)*

``Ada.Containers.Indefinite_Holders`` *(A.18.18)*

``Ada.Containers.Indefinite_Multiway_Trees`` *(A.18.17)*

``Ada.Containers.Indefinite_Ordered_Maps`` *(A.18.14)*

``Ada.Containers.Indefinite_Ordered_Sets`` *(A.18.16)*

``Ada.Containers.Indefinite_Vectors`` *(A.18.11)*

``Ada.Containers.Multiway_Trees`` *(A.18.10)*

``Ada.Containers.Ordered_Maps`` *(A.18.6)*

``Ada.Containers.Ordered_Sets`` *(A.18.9)*

``Ada.Containers.Synchronized_Queue_Interfaces`` *(A.18.27)*

``Ada.Containers.Unbounded_Priority_Queues`` *(A.18.30)*

``Ada.Containers.Unbounded_Synchronized_Queues`` *(A.18.28)*

``Ada.Containers.Vectors`` *(A.18.2)*

``Ada.Directories`` *(A.16)*
  This package provides operations on directories.


``Ada.Directories.Hierarchical_File_Names`` *(A.16.1)*
  This package provides additional directory operations handling
  hierarchical file names.


``Ada.Directories.Information`` *(A.16)*
  This is an implementation defined package for additional directory
  operations, which is not implemented in GNAT.


``Ada.Decimal`` *(F.2)*
  This package provides constants describing the range of decimal numbers
  implemented, and also a decimal divide routine (analogous to the COBOL
  verb DIVIDE ... GIVING ... REMAINDER ...)


``Ada.Direct_IO`` *(A.8.4)*
  This package provides input-output using a model of a set of records of
  fixed-length, containing an arbitrary definite Ada type, indexed by an
  integer record number.


``Ada.Dispatching`` *(D.2.1)*
  A parent package containing definitions for task dispatching operations.


``Ada.Dispatching.EDF`` *(D.2.6)*
  Not implemented in GNAT.


``Ada.Dispatching.Non_Preemptive`` *(D.2.4)*
  Not implemented in GNAT.


``Ada.Dispatching.Round_Robin`` *(D.2.5)*
  Not implemented in GNAT.


``Ada.Dynamic_Priorities`` *(D.5)*
  This package allows the priorities of a task to be adjusted dynamically
  as the task is running.


``Ada.Environment_Variables`` *(A.17)*
  This package provides facilities for accessing environment variables.


``Ada.Exceptions`` *(11.4.1)*
  This package provides additional information on exceptions, and also
  contains facilities for treating exceptions as data objects, and raising
  exceptions with associated messages.


``Ada.Execution_Time`` *(D.14)*
  This package provides CPU clock functionalities. It is not implemented on
  all targets (see package spec for details).


``Ada.Execution_Time.Group_Budgets`` *(D.14.2)*
  Not implemented in GNAT.


``Ada.Execution_Time.Timers`` *(D.14.1)'*
  Not implemented in GNAT.


``Ada.Finalization`` *(7.6)*
  This package contains the declarations and subprograms to support the
  use of controlled types, providing for automatic initialization and
  finalization (analogous to the constructors and destructors of C++).


``Ada.Float_Text_IO`` *(A.10.9)*
  A library level instantiation of Text_IO.Float_IO for type Float.


``Ada.Float_Wide_Text_IO`` *(A.10.9)*
  A library level instantiation of Wide_Text_IO.Float_IO for type Float.


``Ada.Float_Wide_Wide_Text_IO`` *(A.10.9)*
  A library level instantiation of Wide_Wide_Text_IO.Float_IO for type Float.


``Ada.Integer_Text_IO`` *(A.10.9)*
  A library level instantiation of Text_IO.Integer_IO for type Integer.


``Ada.Integer_Wide_Text_IO`` *(A.10.9)*
  A library level instantiation of Wide_Text_IO.Integer_IO for type Integer.


``Ada.Integer_Wide_Wide_Text_IO`` *(A.10.9)*
  A library level instantiation of Wide_Wide_Text_IO.Integer_IO for type Integer.


``Ada.Interrupts`` *(C.3.2)*
  This package provides facilities for interfacing to interrupts, which
  includes the set of signals or conditions that can be raised and
  recognized as interrupts.


``Ada.Interrupts.Names`` *(C.3.2)*
  This package provides the set of interrupt names (actually signal
  or condition names) that can be handled by GNAT.


``Ada.IO_Exceptions`` *(A.13)*
  This package defines the set of exceptions that can be raised by use of
  the standard IO packages.


``Ada.Iterator_Interfaces`` *(5.5.1)*
  This package provides a generic interface to generalized iterators.


``Ada.Locales`` *(A.19)*
  This package provides declarations providing information (Language
  and Country) about the current locale.


``Ada.Numerics``
  This package contains some standard constants and exceptions used
  throughout the numerics packages.  Note that the constants pi and e are
  defined here, and it is better to use these definitions than rolling
  your own.


``Ada.Numerics.Complex_Arrays`` *(G.3.2)*
  Provides operations on arrays of complex numbers.


``Ada.Numerics.Complex_Elementary_Functions``
  Provides the implementation of standard elementary functions (such as
  log and trigonometric functions) operating on complex numbers using the
  standard ``Float`` and the ``Complex`` and ``Imaginary`` types
  created by the package ``Numerics.Complex_Types``.


``Ada.Numerics.Complex_Types``
  This is a predefined instantiation of
  ``Numerics.Generic_Complex_Types`` using ``Standard.Float`` to
  build the type ``Complex`` and ``Imaginary``.


``Ada.Numerics.Discrete_Random``
  This generic package provides a random number generator suitable for generating
  uniformly distributed values of a specified discrete subtype. It should not be
  used as a cryptographic pseudo-random source.


``Ada.Numerics.Float_Random``
  This package provides a random number generator suitable for generating
  uniformly distributed floating point values in the unit interval. It should not
  be used as a cryptographic pseudo-random source.


``Ada.Numerics.Generic_Complex_Elementary_Functions``
  This is a generic version of the package that provides the
  implementation of standard elementary functions (such as log and
  trigonometric functions) for an arbitrary complex type.

  The following predefined instantiations of this package are provided:

  * ``Short_Float``

    ``Ada.Numerics.Short_Complex_Elementary_Functions``

  * ``Float``

    ``Ada.Numerics.Complex_Elementary_Functions``

  * ``Long_Float``

    ``Ada.Numerics.Long_Complex_Elementary_Functions``

``Ada.Numerics.Generic_Complex_Types``
  This is a generic package that allows the creation of complex types,
  with associated complex arithmetic operations.

  The following predefined instantiations of this package exist

  * ``Short_Float``

    ``Ada.Numerics.Short_Complex_Complex_Types``

  * ``Float``

    ``Ada.Numerics.Complex_Complex_Types``

  * ``Long_Float``

    ``Ada.Numerics.Long_Complex_Complex_Types``

``Ada.Numerics.Generic_Elementary_Functions``
  This is a generic package that provides the implementation of standard
  elementary functions (such as log an trigonometric functions) for an
  arbitrary float type.

  The following predefined instantiations of this package exist

  * ``Short_Float``

    ``Ada.Numerics.Short_Elementary_Functions``

  * ``Float``

    ``Ada.Numerics.Elementary_Functions``

  * ``Long_Float``

    ``Ada.Numerics.Long_Elementary_Functions``

``Ada.Numerics.Generic_Real_Arrays`` *(G.3.1)*
  Generic operations on arrays of reals

``Ada.Numerics.Real_Arrays`` *(G.3.1)*
  Preinstantiation of Ada.Numerics.Generic_Real_Arrays (Float).

``Ada.Real_Time`` *(D.8)*
  This package provides facilities similar to those of ``Calendar``, but
  operating with a finer clock suitable for real time control. Note that
  annex D requires that there be no backward clock jumps, and GNAT generally
  guarantees this behavior, but of course if the external clock on which
  the GNAT runtime depends is deliberately reset by some external event,
  then such a backward jump may occur.

``Ada.Real_Time.Timing_Events`` *(D.15)*
  This package allows procedures to be executed at a specified time without
  the use of a task or a delay statement.

``Ada.Sequential_IO`` *(A.8.1)*
  This package provides input-output facilities for sequential files,
  which can contain a sequence of values of a single type, which can be
  any Ada type, including indefinite (unconstrained) types.

``Ada.Storage_IO`` *(A.9)*
  This package provides a facility for mapping arbitrary Ada types to and
  from a storage buffer.  It is primarily intended for the creation of new
  IO packages.

``Ada.Streams`` *(13.13.1)*
  This is a generic package that provides the basic support for the
  concept of streams as used by the stream attributes (``Input``,
  ``Output``, ``Read`` and ``Write``).

``Ada.Streams.Stream_IO`` *(A.12.1)*
  This package is a specialization of the type ``Streams`` defined in
  package ``Streams`` together with a set of operations providing
  Stream_IO capability.  The Stream_IO model permits both random and
  sequential access to a file which can contain an arbitrary set of values
  of one or more Ada types.

``Ada.Strings`` *(A.4.1)*
  This package provides some basic constants used by the string handling
  packages.


``Ada.Strings.Bounded`` *(A.4.4)*
  This package provides facilities for handling variable length
  strings.  The bounded model requires a maximum length.  It is thus
  somewhat more limited than the unbounded model, but avoids the use of
  dynamic allocation or finalization.

``Ada.Strings.Bounded.Equal_Case_Insensitive`` *(A.4.10)*
  Provides case-insensitive comparisons of bounded strings

``Ada.Strings.Bounded.Hash`` *(A.4.9)*
  This package provides a generic hash function for bounded strings

``Ada.Strings.Bounded.Hash_Case_Insensitive`` *(A.4.9)*
  This package provides a generic hash function for bounded strings that
  converts the string to be hashed to lower case.

``Ada.Strings.Bounded.Less_Case_Insensitive`` *(A.4.10)*
  This package provides a comparison function for bounded strings that works
  in a case insensitive manner by converting to lower case before the comparison.

``Ada.Strings.Fixed`` *(A.4.3)*
  This package provides facilities for handling fixed length strings.

``Ada.Strings.Fixed.Equal_Case_Insensitive`` *(A.4.10)*
  This package provides an equality function for fixed strings that compares
  the strings after converting both to lower case.

``Ada.Strings.Fixed.Hash_Case_Insensitive`` *(A.4.9)*
  This package provides a case insensitive hash function for fixed strings that
  converts the string to lower case before computing the hash.

``Ada.Strings.Fixed.Less_Case_Insensitive`` *(A.4.10)*
  This package provides a comparison function for fixed strings that works
  in a case insensitive manner by converting to lower case before the comparison.

``Ada.Strings.Hash`` *(A.4.9)*
  This package provides a hash function for strings.

``Ada.Strings.Hash_Case_Insensitive`` *(A.4.9)*
  This package provides a hash function for strings that is case insensitive.
  The string is converted to lower case before computing the hash.

``Ada.Strings.Less_Case_Insensitive`` *(A.4.10)*
  This package provides a comparison function for\\strings that works
  in a case insensitive manner by converting to lower case before the comparison.

``Ada.Strings.Maps`` *(A.4.2)*
  This package provides facilities for handling character mappings and
  arbitrarily defined subsets of characters.  For instance it is useful in
  defining specialized translation tables.

``Ada.Strings.Maps.Constants`` *(A.4.6)*
  This package provides a standard set of predefined mappings and
  predefined character sets.  For example, the standard upper to lower case
  conversion table is found in this package.  Note that upper to lower case
  conversion is non-trivial if you want to take the entire set of
  characters, including extended characters like E with an acute accent,
  into account.  You should use the mappings in this package (rather than
  adding 32 yourself) to do case mappings.

``Ada.Strings.Unbounded`` *(A.4.5)*
  This package provides facilities for handling variable length
  strings.  The unbounded model allows arbitrary length strings, but
  requires the use of dynamic allocation and finalization.

``Ada.Strings.Unbounded.Equal_Case_Insensitive`` *(A.4.10)*
  Provides case-insensitive comparisons of unbounded strings

``Ada.Strings.Unbounded.Hash`` *(A.4.9)*
  This package provides a generic hash function for unbounded strings

``Ada.Strings.Unbounded.Hash_Case_Insensitive`` *(A.4.9)*
  This package provides a generic hash function for unbounded strings that
  converts the string to be hashed to lower case.

``Ada.Strings.Unbounded.Less_Case_Insensitive`` *(A.4.10)*
  This package provides a comparison function for unbounded strings that works
  in a case insensitive manner by converting to lower case before the comparison.

``Ada.Strings.UTF_Encoding`` *(A.4.11)*
  This package provides basic definitions for dealing with UTF-encoded strings.

``Ada.Strings.UTF_Encoding.Conversions`` *(A.4.11)*
  This package provides conversion functions for UTF-encoded strings.

``Ada.Strings.UTF_Encoding.Strings`` *(A.4.11)*

``Ada.Strings.UTF_Encoding.Wide_Strings`` *(A.4.11)*

``Ada.Strings.UTF_Encoding.Wide_Wide_Strings`` *(A.4.11)*
  These packages provide facilities for handling UTF encodings for
  Strings, Wide_Strings and Wide_Wide_Strings.

``Ada.Strings.Wide_Bounded`` *(A.4.7)*

``Ada.Strings.Wide_Fixed`` *(A.4.7)*

``Ada.Strings.Wide_Maps`` *(A.4.7)*

``Ada.Strings.Wide_Unbounded`` *(A.4.7)*
  These packages provide analogous capabilities to the corresponding
  packages without ``Wide_`` in the name, but operate with the types
  ``Wide_String`` and ``Wide_Character`` instead of ``String``
  and ``Character``. Versions of all the child packages are available.

``Ada.Strings.Wide_Wide_Bounded`` *(A.4.7)*

``Ada.Strings.Wide_Wide_Fixed`` *(A.4.7)*

``Ada.Strings.Wide_Wide_Maps`` *(A.4.7)*

``Ada.Strings.Wide_Wide_Unbounded`` *(A.4.7)*
  These packages provide analogous capabilities to the corresponding
  packages without ``Wide_`` in the name, but operate with the types
  ``Wide_Wide_String`` and ``Wide_Wide_Character`` instead
  of ``String`` and ``Character``.

``Ada.Synchronous_Barriers`` *(D.10.1)*
  This package provides facilities for synchronizing tasks at a low level
  with barriers.

``Ada.Synchronous_Task_Control`` *(D.10)*
  This package provides some standard facilities for controlling task
  communication in a synchronous manner.

``Ada.Synchronous_Task_Control.EDF`` *(D.10)*
  Not implemented in GNAT.

``Ada.Tags``
  This package contains definitions for manipulation of the tags of tagged
  values.

``Ada.Tags.Generic_Dispatching_Constructor`` *(3.9)*
  This package provides a way of constructing tagged class-wide values given
  only the tag value.

``Ada.Task_Attributes`` *(C.7.2)*
  This package provides the capability of associating arbitrary
  task-specific data with separate tasks.

``Ada.Task_Identification`` *(C.7.1)*
  This package provides capabilities for task identification.

``Ada.Task_Termination`` *(C.7.3)*
  This package provides control over task termination.

``Ada.Text_IO``
  This package provides basic text input-output capabilities for
  character, string and numeric data.  The subpackages of this
  package are listed next. Note that although these are defined
  as subpackages in the RM, they are actually transparently
  implemented as child packages in GNAT, meaning that they
  are only loaded if needed.

``Ada.Text_IO.Decimal_IO``
  Provides input-output facilities for decimal fixed-point types

``Ada.Text_IO.Enumeration_IO``
  Provides input-output facilities for enumeration types.

``Ada.Text_IO.Fixed_IO``
  Provides input-output facilities for ordinary fixed-point types.

``Ada.Text_IO.Float_IO``
  Provides input-output facilities for float types.  The following
  predefined instantiations of this generic package are available:

  * ``Short_Float``

    ``Short_Float_Text_IO``

  * ``Float``

    ``Float_Text_IO``

  * ``Long_Float``

    ``Long_Float_Text_IO``

``Ada.Text_IO.Integer_IO``
  Provides input-output facilities for integer types.  The following
  predefined instantiations of this generic package are available:

  * ``Short_Short_Integer``

    ``Ada.Short_Short_Integer_Text_IO``

  * ``Short_Integer``

    ``Ada.Short_Integer_Text_IO``

  * ``Integer``

    ``Ada.Integer_Text_IO``

  * ``Long_Integer``

    ``Ada.Long_Integer_Text_IO``

  * ``Long_Long_Integer``

    ``Ada.Long_Long_Integer_Text_IO``

``Ada.Text_IO.Modular_IO``
  Provides input-output facilities for modular (unsigned) types.

``Ada.Text_IO.Bounded_IO (A.10.11)``
  Provides input-output facilities for bounded strings.

``Ada.Text_IO.Complex_IO (G.1.3)``
  This package provides basic text input-output capabilities for complex
  data.

``Ada.Text_IO.Editing (F.3.3)``
  This package contains routines for edited output, analogous to the use
  of pictures in COBOL.  The picture formats used by this package are a
  close copy of the facility in COBOL.

``Ada.Text_IO.Text_Streams (A.12.2)``
  This package provides a facility that allows Text_IO files to be treated
  as streams, so that the stream attributes can be used for writing
  arbitrary data, including binary data, to Text_IO files.

``Ada.Text_IO.Unbounded_IO (A.10.12)``
  This package provides input-output facilities for unbounded strings.

``Ada.Unchecked_Conversion (13.9)``
  This generic package allows arbitrary conversion from one type to
  another of the same size, providing for breaking the type safety in
  special circumstances.

  If the types have the same Size (more accurately the same Value_Size),
  then the effect is simply to transfer the bits from the source to the
  target type without any modification.  This usage is well defined, and
  for simple types whose representation is typically the same across
  all implementations, gives a portable method of performing such
  conversions.

  If the types do not have the same size, then the result is implementation
  defined, and thus may be non-portable.  The following describes how GNAT
  handles such unchecked conversion cases.

  If the types are of different sizes, and are both discrete types, then
  the effect is of a normal type conversion without any constraint checking.
  In particular if the result type has a larger size, the result will be
  zero or sign extended.  If the result type has a smaller size, the result
  will be truncated by ignoring high order bits.

  If the types are of different sizes, and are not both discrete types,
  then the conversion works as though pointers were created to the source
  and target, and the pointer value is converted.  The effect is that bits
  are copied from successive low order storage units and bits of the source
  up to the length of the target type.

  A warning is issued if the lengths differ, since the effect in this
  case is implementation dependent, and the above behavior may not match
  that of some other compiler.

  A pointer to one type may be converted to a pointer to another type using
  unchecked conversion.  The only case in which the effect is undefined is
  when one or both pointers are pointers to unconstrained array types.  In
  this case, the bounds information may get incorrectly transferred, and in
  particular, GNAT uses double size pointers for such types, and it is
  meaningless to convert between such pointer types.  GNAT will issue a
  warning if the alignment of the target designated type is more strict
  than the alignment of the source designated type (since the result may
  be unaligned in this case).

  A pointer other than a pointer to an unconstrained array type may be
  converted to and from System.Address.  Such usage is common in Ada 83
  programs, but note that Ada.Address_To_Access_Conversions is the
  preferred method of performing such conversions in Ada 95 and Ada 2005.
  Neither
  unchecked conversion nor Ada.Address_To_Access_Conversions should be
  used in conjunction with pointers to unconstrained objects, since
  the bounds information cannot be handled correctly in this case.

``Ada.Unchecked_Deallocation`` *(13.11.2)*
  This generic package allows explicit freeing of storage previously
  allocated by use of an allocator.

``Ada.Wide_Text_IO`` *(A.11)*
  This package is similar to ``Ada.Text_IO``, except that the external
  file supports wide character representations, and the internal types are
  ``Wide_Character`` and ``Wide_String`` instead of ``Character``
  and ``String``. The corresponding set of nested packages and child
  packages are defined.

``Ada.Wide_Wide_Text_IO`` *(A.11)*
  This package is similar to ``Ada.Text_IO``, except that the external
  file supports wide character representations, and the internal types are
  ``Wide_Character`` and ``Wide_String`` instead of ``Character``
  and ``String``. The corresponding set of nested packages and child
  packages are defined.

For packages in Interfaces and System, all the RM defined packages are
available in GNAT, see the Ada 2012 RM for full details.
