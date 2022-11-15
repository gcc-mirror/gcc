..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: machine modes, machine_mode

.. _machine-modes:

Machine Modes
*************

A machine mode describes a size of data object and the representation used
for it.  In the C code, machine modes are represented by an enumeration
type, ``machine_mode``, defined in :samp:`machmode.def`.  Each RTL
expression has room for a machine mode and so do certain kinds of tree
expressions (declarations and types, to be precise).

In debugging dumps and machine descriptions, the machine mode of an RTL
expression is written after the expression code with a colon to separate
them.  The letters :samp:`mode` which appear at the end of each machine mode
name are omitted.  For example, ``(reg:SI 38)`` is a ``reg``
expression with machine mode ``SImode``.  If the mode is
``VOIDmode``, it is not written at all.

Here is a table of machine modes.  The term 'byte' below refers to an
object of ``BITS_PER_UNIT`` bits (see :ref:`storage-layout`).

.. index:: BImode

``BImode``
  'Bit' mode represents a single bit, for predicate registers.

  .. index:: QImode

``QImode``
  'Quarter-Integer' mode represents a single byte treated as an integer.

  .. index:: HImode

``HImode``
  'Half-Integer' mode represents a two-byte integer.

  .. index:: PSImode

``PSImode``
  'Partial Single Integer' mode represents an integer which occupies
  four bytes but which doesn't really use all four.  On some machines,
  this is the right mode to use for pointers.

  .. index:: SImode

``SImode``
  'Single Integer' mode represents a four-byte integer.

  .. index:: PDImode

``PDImode``
  'Partial Double Integer' mode represents an integer which occupies
  eight bytes but which doesn't really use all eight.  On some machines,
  this is the right mode to use for certain pointers.

  .. index:: DImode

``DImode``
  'Double Integer' mode represents an eight-byte integer.

  .. index:: TImode

``TImode``
  'Tetra Integer' (?) mode represents a sixteen-byte integer.

  .. index:: OImode

``OImode``
  'Octa Integer' (?) mode represents a thirty-two-byte integer.

  .. index:: XImode

``XImode``
  'Hexadeca Integer' (?) mode represents a sixty-four-byte integer.

  .. index:: QFmode

``QFmode``
  'Quarter-Floating' mode represents a quarter-precision (single byte)
  floating point number.

  .. index:: HFmode

``HFmode``
  'Half-Floating' mode represents a half-precision (two byte) floating
  point number.

  .. index:: TQFmode

``TQFmode``
  'Three-Quarter-Floating' (?) mode represents a three-quarter-precision
  (three byte) floating point number.

  .. index:: SFmode

``SFmode``
  'Single Floating' mode represents a four byte floating point number.
  In the common case, of a processor with IEEE arithmetic and 8-bit bytes,
  this is a single-precision IEEE floating point number; it can also be
  used for double-precision (on processors with 16-bit bytes) and
  single-precision VAX and IBM types.

  .. index:: DFmode

``DFmode``
  'Double Floating' mode represents an eight byte floating point number.
  In the common case, of a processor with IEEE arithmetic and 8-bit bytes,
  this is a double-precision IEEE floating point number.

  .. index:: XFmode

``XFmode``
  'Extended Floating' mode represents an IEEE extended floating point
  number.  This mode only has 80 meaningful bits (ten bytes).  Some
  processors require such numbers to be padded to twelve bytes, others
  to sixteen; this mode is used for either.

  .. index:: SDmode

``SDmode``
  'Single Decimal Floating' mode represents a four byte decimal
  floating point number (as distinct from conventional binary floating
  point).

  .. index:: DDmode

``DDmode``
  'Double Decimal Floating' mode represents an eight byte decimal
  floating point number.

  .. index:: TDmode

``TDmode``
  'Tetra Decimal Floating' mode represents a sixteen byte decimal
  floating point number all 128 of whose bits are meaningful.

  .. index:: TFmode

``TFmode``
  'Tetra Floating' mode represents a sixteen byte floating point number
  all 128 of whose bits are meaningful.  One common use is the
  IEEE quad-precision format.

  .. index:: QQmode

``QQmode``
  'Quarter-Fractional' mode represents a single byte treated as a signed
  fractional number.  The default format is 's.7'.

  .. index:: HQmode

``HQmode``
  'Half-Fractional' mode represents a two-byte signed fractional number.
  The default format is 's.15'.

  .. index:: SQmode

``SQmode``
  'Single Fractional' mode represents a four-byte signed fractional number.
  The default format is 's.31'.

  .. index:: DQmode

``DQmode``
  'Double Fractional' mode represents an eight-byte signed fractional number.
  The default format is 's.63'.

  .. index:: TQmode

``TQmode``
  'Tetra Fractional' mode represents a sixteen-byte signed fractional number.
  The default format is 's.127'.

  .. index:: UQQmode

``UQQmode``
  'Unsigned Quarter-Fractional' mode represents a single byte treated as an
  unsigned fractional number.  The default format is '.8'.

  .. index:: UHQmode

``UHQmode``
  'Unsigned Half-Fractional' mode represents a two-byte unsigned fractional
  number.  The default format is '.16'.

  .. index:: USQmode

``USQmode``
  'Unsigned Single Fractional' mode represents a four-byte unsigned fractional
  number.  The default format is '.32'.

  .. index:: UDQmode

``UDQmode``
  'Unsigned Double Fractional' mode represents an eight-byte unsigned
  fractional number.  The default format is '.64'.

  .. index:: UTQmode

``UTQmode``
  'Unsigned Tetra Fractional' mode represents a sixteen-byte unsigned
  fractional number.  The default format is '.128'.

  .. index:: HAmode

``HAmode``
  'Half-Accumulator' mode represents a two-byte signed accumulator.
  The default format is 's8.7'.

  .. index:: SAmode

``SAmode``
  'Single Accumulator' mode represents a four-byte signed accumulator.
  The default format is 's16.15'.

  .. index:: DAmode

``DAmode``
  'Double Accumulator' mode represents an eight-byte signed accumulator.
  The default format is 's32.31'.

  .. index:: TAmode

``TAmode``
  'Tetra Accumulator' mode represents a sixteen-byte signed accumulator.
  The default format is 's64.63'.

  .. index:: UHAmode

``UHAmode``
  'Unsigned Half-Accumulator' mode represents a two-byte unsigned accumulator.
  The default format is '8.8'.

  .. index:: USAmode

``USAmode``
  'Unsigned Single Accumulator' mode represents a four-byte unsigned
  accumulator.  The default format is '16.16'.

  .. index:: UDAmode

``UDAmode``
  'Unsigned Double Accumulator' mode represents an eight-byte unsigned
  accumulator.  The default format is '32.32'.

  .. index:: UTAmode

``UTAmode``
  'Unsigned Tetra Accumulator' mode represents a sixteen-byte unsigned
  accumulator.  The default format is '64.64'.

  .. index:: CCmode

``CCmode``
  'Condition Code' mode represents the value of a condition code, which
  is a machine-specific set of bits used to represent the result of a
  comparison operation.  Other machine-specific modes may also be used for
  the condition code.  (see :ref:`condition-code`).

  .. index:: BLKmode

``BLKmode``
  'Block' mode represents values that are aggregates to which none of
  the other modes apply.  In RTL, only memory references can have this mode,
  and only if they appear in string-move or vector instructions.  On machines
  which have no such instructions, ``BLKmode`` will not appear in RTL.

  .. index:: VOIDmode

``VOIDmode``
  Void mode means the absence of a mode or an unspecified mode.
  For example, RTL expressions of code ``const_int`` have mode
  ``VOIDmode`` because they can be taken to have whatever mode the context
  requires.  In debugging dumps of RTL, ``VOIDmode`` is expressed by
  the absence of any mode.

  .. index:: QCmode, HCmode, SCmode, DCmode, XCmode, TCmode

``QCmode, HCmode, SCmode, DCmode, XCmode, TCmode``
  These modes stand for a complex number represented as a pair of floating
  point values.  The floating point values are in ``QFmode``,
  ``HFmode``, ``SFmode``, ``DFmode``, ``XFmode``, and
  ``TFmode``, respectively.

  .. index:: CQImode, CHImode, CSImode, CDImode, CTImode, COImode, CPSImode

``CQImode, CHImode, CSImode, CDImode, CTImode, COImode, CPSImode``
  These modes stand for a complex number represented as a pair of integer
  values.  The integer values are in ``QImode``, ``HImode``,
  ``SImode``, ``DImode``, ``TImode``, ``OImode``, and ``PSImode``,
  respectively.

  .. index:: BND32mode, BND64mode

``BND32mode BND64mode``
  These modes stand for bounds for pointer of 32 and 64 bit size respectively.
  Mode size is double pointer mode size.

The machine description defines ``Pmode`` as a C macro which expands
into the machine mode used for addresses.  Normally this is the mode
whose size is ``BITS_PER_WORD``, ``SImode`` on 32-bit machines.

The only modes which a machine description must support are
``QImode``, and the modes corresponding to ``BITS_PER_WORD``,
``FLOAT_TYPE_SIZE`` and ``DOUBLE_TYPE_SIZE``.
The compiler will attempt to use ``DImode`` for 8-byte structures and
unions, but this can be prevented by overriding the definition of
``MAX_FIXED_MODE_SIZE``.  Alternatively, you can have the compiler
use ``TImode`` for 16-byte structures and unions.  Likewise, you can
arrange for the C type ``short int`` to avoid using ``HImode``.

.. index:: mode classes

Very few explicit references to machine modes remain in the compiler and
these few references will soon be removed.  Instead, the machine modes
are divided into mode classes.  These are represented by the enumeration
type ``enum mode_class`` defined in :samp:`machmode.h`.  The possible
mode classes are:

.. index:: MODE_INT

.. envvar:: MODE_INT

  Integer modes.  By default these are ``BImode``, ``QImode``,
  ``HImode``, ``SImode``, ``DImode``, ``TImode``, and
  ``OImode``.

.. envvar:: MODE_PARTIAL_INT

  The 'partial integer' modes, ``PQImode``, ``PHImode``,
  ``PSImode`` and ``PDImode``.

.. envvar:: MODE_FLOAT

  Floating point modes.  By default these are ``QFmode``,
  ``HFmode``, ``TQFmode``, ``SFmode``, ``DFmode``,
  ``XFmode`` and ``TFmode``.

.. envvar:: MODE_DECIMAL_FLOAT

  Decimal floating point modes.  By default these are ``SDmode``,
  ``DDmode`` and ``TDmode``.

.. envvar:: MODE_FRACT

  Signed fractional modes.  By default these are ``QQmode``, ``HQmode``,
  ``SQmode``, ``DQmode`` and ``TQmode``.

.. envvar:: MODE_UFRACT

  Unsigned fractional modes.  By default these are ``UQQmode``, ``UHQmode``,
  ``USQmode``, ``UDQmode`` and ``UTQmode``.

.. envvar:: MODE_ACCUM

  Signed accumulator modes.  By default these are ``HAmode``,
  ``SAmode``, ``DAmode`` and ``TAmode``.

.. envvar:: MODE_UACCUM

  Unsigned accumulator modes.  By default these are ``UHAmode``,
  ``USAmode``, ``UDAmode`` and ``UTAmode``.

.. envvar:: MODE_COMPLEX_INT

  Complex integer modes.  (These are not currently implemented).

.. envvar:: MODE_COMPLEX_FLOAT

  Complex floating point modes.  By default these are ``QCmode``,
  ``HCmode``, ``SCmode``, ``DCmode``, ``XCmode``, and
  ``TCmode``.

.. envvar:: MODE_CC

  Modes representing condition code values.  These are ``CCmode`` plus
  any ``CC_MODE`` modes listed in the :samp:`{machine}-modes.def`.
  See :ref:`jump-patterns`,
  also see :ref:`condition-code`.

.. envvar:: MODE_POINTER_BOUNDS

  Pointer bounds modes.  Used to represent values of pointer bounds type.
  Operations in these modes may be executed as NOPs depending on hardware
  features and environment setup.

.. envvar:: MODE_OPAQUE

  This is a mode class for modes that don't want to provide operations
  other than register moves, memory moves, loads, stores, and
  ``unspec`` s. They have a size and precision and that's all.

.. envvar:: MODE_RANDOM

  This is a catchall mode class for modes which don't fit into the above
  classes.  Currently ``VOIDmode`` and ``BLKmode`` are in
  ``MODE_RANDOM``.

.. index:: machine mode wrapper classes

``machmode.h`` also defines various wrapper classes that combine a
``machine_mode`` with a static assertion that a particular
condition holds.  The classes are:

.. index:: scalar_int_mode

``scalar_int_mode``
  A mode that has class ``MODE_INT`` or ``MODE_PARTIAL_INT``.

  .. index:: scalar_float_mode

``scalar_float_mode``
  A mode that has class ``MODE_FLOAT`` or ``MODE_DECIMAL_FLOAT``.

  .. index:: scalar_mode

``scalar_mode``
  A mode that holds a single numerical value.  In practice this means
  that the mode is a ``scalar_int_mode``, is a ``scalar_float_mode``,
  or has class ``MODE_FRACT``, ``MODE_UFRACT``, ``MODE_ACCUM``,
  ``MODE_UACCUM`` or ``MODE_POINTER_BOUNDS``.

  .. index:: complex_mode

``complex_mode``
  A mode that has class ``MODE_COMPLEX_INT`` or ``MODE_COMPLEX_FLOAT``.

  .. index:: fixed_size_mode

``fixed_size_mode``
  A mode whose size is known at compile time.

Named modes use the most constrained of the available wrapper classes,
if one exists, otherwise they use ``machine_mode``.  For example,
``QImode`` is a ``scalar_int_mode``, ``SFmode`` is a
``scalar_float_mode`` and ``BLKmode`` is a plain
``machine_mode``.  It is possible to refer to any mode as a raw
``machine_mode`` by adding the ``E_`` prefix, where ``E``
stands for 'enumeration'.  For example, the raw ``machine_mode``
names of the modes just mentioned are ``E_QImode``, ``E_SFmode``
and ``E_BLKmode`` respectively.

The wrapper classes implicitly convert to ``machine_mode`` and to any
wrapper class that represents a more general condition; for example
``scalar_int_mode`` and ``scalar_float_mode`` both convert
to ``scalar_mode`` and all three convert to ``fixed_size_mode``.
The classes act like ``machine_mode`` s that accept only certain
named modes.

.. index:: opt_mode

:samp:`machmode.h` also defines a template class ``opt_mode<T>``
that holds a ``T`` or nothing, where ``T`` can be either
``machine_mode`` or one of the wrapper classes above.  The main
operations on an ``opt_mode<T>`` :samp:`{x}` are as follows:

:samp:`{x}.exists ()`
  Return true if :samp:`{x}` holds a mode rather than nothing.

:samp:`{x}.exists (&{y})`
  Return true if :samp:`{x}` holds a mode rather than nothing, storing the
  mode in :samp:`{y}` if so.  :samp:`{y}` must be assignment-compatible with :samp:`{T}`.

:samp:`{x}.require ()`
  Assert that :samp:`{x}` holds a mode rather than nothing and return that mode.

:samp:`{x} = {y}`
  Set :samp:`{x}` to :samp:`{y}`, where :samp:`{y}` is a :samp:`{T}` or implicitly converts
  to a :samp:`{T}`.

The default constructor sets an ``opt_mode<T>`` to nothing.
There is also a constructor that takes an initial value of type :samp:`{T}`.

It is possible to use the :samp:`is-a.h` accessors on a ``machine_mode``
or machine mode wrapper :samp:`{x}` :

.. index:: is_a

:samp:`is_a <{T}> ({x})`
  Return true if :samp:`{x}` meets the conditions for wrapper class :samp:`{T}`.

:samp:`is_a <{T}> ({x}, &{y})`
  Return true if :samp:`{x}` meets the conditions for wrapper class :samp:`{T}`,
  storing it in :samp:`{y}` if so.  :samp:`{y}` must be assignment-compatible with
  :samp:`{T}`.

:samp:`as_a <{T}> ({x})`
  Assert that :samp:`{x}` meets the conditions for wrapper class :samp:`{T}`
  and return it as a :samp:`{T}`.

:samp:`dyn_cast <{T}> ({x})`
  Return an ``opt_mode<T>`` that holds :samp:`{x}` if :samp:`{x}` meets
  the conditions for wrapper class :samp:`{T}` and that holds nothing otherwise.

The purpose of these wrapper classes is to give stronger static type
checking.  For example, if a function takes a ``scalar_int_mode``,
a caller that has a general ``machine_mode`` must either check or
assert that the code is indeed a scalar integer first, using one of
the functions above.

The wrapper classes are normal C++ classes, with user-defined
constructors.  Sometimes it is useful to have a POD version of
the same type, particularly if the type appears in a ``union``.
The template class ``pod_mode<T>`` provides a POD version
of wrapper class :samp:`{T}`.  It is assignment-compatible with :samp:`{T}`
and implicitly converts to both ``machine_mode`` and :samp:`{T}`.

Here are some C macros that relate to machine modes:

.. index:: GET_MODE

:samp:`GET_MODE ({x})`
  Returns the machine mode of the RTX :samp:`{x}`.

  .. index:: PUT_MODE

:samp:`PUT_MODE ({x}, {newmode})`
  Alters the machine mode of the RTX :samp:`{x}` to be :samp:`{newmode}`.

  .. index:: NUM_MACHINE_MODES

.. envvar:: NUM_MACHINE_MODES

  Stands for the number of machine modes available on the target
  machine.  This is one greater than the largest numeric value of any
  machine mode.

:samp:`GET_MODE_NAME ({m})`
  Returns the name of mode :samp:`{m}` as a string.

  .. index:: GET_MODE_CLASS

:samp:`GET_MODE_CLASS ({m})`
  Returns the mode class of mode :samp:`{m}`.

  .. index:: GET_MODE_WIDER_MODE

:samp:`GET_MODE_WIDER_MODE ({m})`
  Returns the next wider natural mode.  For example, the expression
  ``GET_MODE_WIDER_MODE (QImode)`` returns ``HImode``.

  .. index:: GET_MODE_SIZE

:samp:`GET_MODE_SIZE ({m})`
  Returns the size in bytes of a datum of mode :samp:`{m}`.

  .. index:: GET_MODE_BITSIZE

:samp:`GET_MODE_BITSIZE ({m})`
  Returns the size in bits of a datum of mode :samp:`{m}`.

  .. index:: GET_MODE_IBIT

:samp:`GET_MODE_IBIT ({m})`
  Returns the number of integral bits of a datum of fixed-point mode :samp:`{m}`.

  .. index:: GET_MODE_FBIT

:samp:`GET_MODE_FBIT ({m})`
  Returns the number of fractional bits of a datum of fixed-point mode :samp:`{m}`.

  .. index:: GET_MODE_MASK

:samp:`GET_MODE_MASK ({m})`
  Returns a bitmask containing 1 for all bits in a word that fit within
  mode :samp:`{m}`.  This macro can only be used for modes whose bitsize is
  less than or equal to ``HOST_BITS_PER_INT``.

  .. index:: GET_MODE_ALIGNMENT

:samp:`GET_MODE_ALIGNMENT ({m})`
  Return the required alignment, in bits, for an object of mode :samp:`{m}`.

  .. index:: GET_MODE_UNIT_SIZE

:samp:`GET_MODE_UNIT_SIZE ({m})`
  Returns the size in bytes of the subunits of a datum of mode :samp:`{m}`.
  This is the same as ``GET_MODE_SIZE`` except in the case of complex
  modes.  For them, the unit size is the size of the real or imaginary
  part.

  .. index:: GET_MODE_NUNITS

:samp:`GET_MODE_NUNITS ({m})`
  Returns the number of units contained in a mode, i.e.,
  ``GET_MODE_SIZE`` divided by ``GET_MODE_UNIT_SIZE``.

  .. index:: GET_CLASS_NARROWEST_MODE

:samp:`GET_CLASS_NARROWEST_MODE ({c})`
  Returns the narrowest mode in mode class :samp:`{c}`.

The following 3 variables are defined on every target.   They can be
used to allocate buffers that are guaranteed to be large enough to
hold any value that can be represented on the target.   The first two
can be overridden by defining them in the target's mode.def file,
however, the value must be a constant that can determined very early
in the compilation process.   The third symbol cannot be overridden.

.. index:: BITS_PER_UNIT

.. envvar:: BITS_PER_UNIT

  The number of bits in an addressable storage unit (byte).  If you do
  not define this, the default is 8.

.. envvar:: MAX_BITSIZE_MODE_ANY_INT

  The maximum bitsize of any mode that is used in integer math.  This
  should be overridden by the target if it uses large integers as
  containers for larger vectors but otherwise never uses the contents to
  compute integer values.

.. envvar:: MAX_BITSIZE_MODE_ANY_MODE

  The bitsize of the largest mode on the target.  The default value is
  the largest mode size given in the mode definition file, which is
  always correct for targets whose modes have a fixed size.  Targets
  that might increase the size of a mode beyond this default should define
  ``MAX_BITSIZE_MODE_ANY_MODE`` to the actual upper limit in
  :samp:`{machine}-modes.def`.

.. index:: byte_mode, word_mode

The global variables ``byte_mode`` and ``word_mode`` contain modes
whose classes are ``MODE_INT`` and whose bitsizes are either
``BITS_PER_UNIT`` or ``BITS_PER_WORD``, respectively.  On 32-bit
machines, these are ``QImode`` and ``SImode``, respectively.
