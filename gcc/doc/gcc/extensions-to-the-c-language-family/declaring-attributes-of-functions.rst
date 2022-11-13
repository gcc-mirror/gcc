..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: function attributes, declaring attributes of functions, volatile applied to function, const applied to function

.. _function-attributes:

Declaring Attributes of Functions
*********************************

In GNU C and C++, you can use function attributes to specify certain
function properties that may help the compiler optimize calls or
check code more carefully for correctness.  For example, you
can use attributes to specify that a function never returns
(:fn-attr:`noreturn`), returns a value depending only on the values of
its arguments (``const``), or has ``printf`` -style arguments
(``format``).

You can also use attributes to control memory placement, code
generation options or call/return conventions within the function
being annotated.  Many of these attributes are target-specific.  For
example, many targets support attributes for defining interrupt
handler functions, which typically must follow special register usage
and return conventions.  Such attributes are described in the subsection
for each target.  However, a considerable number of attributes are
supported by most, if not all targets.  Those are described in
the :ref:`common-function-attributes` section.

Function attributes are introduced by the ``__attribute__`` keyword
in the declaration of a function, followed by an attribute specification
enclosed in double parentheses.  You can specify multiple attributes in
a declaration by separating them by commas within the double parentheses
or by immediately following one attribute specification with another.
See :ref:`attribute-syntax`, for the exact rules on attribute syntax and
placement.  Compatible attribute specifications on distinct declarations
of the same function are merged.  An attribute specification that is not
compatible with attributes already applied to a declaration of the same
function is ignored with a warning.

Some function attributes take one or more arguments that refer to
the function's parameters by their positions within the function parameter
list.  Such attribute arguments are referred to as :dfn:`positional arguments`.
Unless specified otherwise, positional arguments that specify properties
of parameters with pointer types can also specify the same properties of
the implicit C++ ``this`` argument in non-static member functions, and
of parameters of reference to a pointer type.  For ordinary functions,
position one refers to the first parameter on the list.  In C++ non-static
member functions, position one refers to the implicit ``this`` pointer.
The same restrictions and effects apply to function attributes used with
ordinary functions or C++ member functions.

GCC also supports attributes on
variable declarations (see :ref:`variable-attributes`),
labels (see :ref:`label-attributes`),
enumerators (see :ref:`enumerator-attributes`),
statements (see :ref:`statement-attributes`),
types (see :ref:`type-attributes`),
and on field declarations (for :fn-attr:`tainted_args`).

There is some overlap between the purposes of attributes and pragmas
(see :ref:`pragmas`).  It has been
found convenient to use ``__attribute__`` to achieve a natural
attachment of attributes to their corresponding declarations, whereas
``#pragma`` is of use for compatibility with other compilers
or constructs that do not naturally form part of the grammar.

In addition to the attributes documented here,
GCC plugins may provide their own attributes.

.. toctree::
  :maxdepth: 1

  declaring-attributes-of-functions/common-function-attributes
  declaring-attributes-of-functions/aarch64-function-attributes
  declaring-attributes-of-functions/amd-gcn-function-attributes
  declaring-attributes-of-functions/arc-function-attributes
  declaring-attributes-of-functions/arm-function-attributes
  declaring-attributes-of-functions/avr-function-attributes
  declaring-attributes-of-functions/blackfin-function-attributes
  declaring-attributes-of-functions/bpf-function-attributes
  declaring-attributes-of-functions/c-sky-function-attributes
  declaring-attributes-of-functions/epiphany-function-attributes
  declaring-attributes-of-functions/h8-300-function-attributes
  declaring-attributes-of-functions/ia-64-function-attributes
  declaring-attributes-of-functions/m32c-function-attributes
  declaring-attributes-of-functions/m32r-d-function-attributes
  declaring-attributes-of-functions/m68k-function-attributes
  declaring-attributes-of-functions/mcore-function-attributes
  declaring-attributes-of-functions/mep-function-attributes
  declaring-attributes-of-functions/microblaze-function-attributes
  declaring-attributes-of-functions/microsoft-windows-function-attributes
  declaring-attributes-of-functions/mips-function-attributes
  declaring-attributes-of-functions/msp430-function-attributes
  declaring-attributes-of-functions/nds32-function-attributes
  declaring-attributes-of-functions/nios-ii-function-attributes
  declaring-attributes-of-functions/nvidia-ptx-function-attributes
  declaring-attributes-of-functions/powerpc-function-attributes
  declaring-attributes-of-functions/risc-v-function-attributes
  declaring-attributes-of-functions/rl78-function-attributes
  declaring-attributes-of-functions/rx-function-attributes
  declaring-attributes-of-functions/s-390-function-attributes
  declaring-attributes-of-functions/sh-function-attributes
  declaring-attributes-of-functions/symbian-os-function-attributes
  declaring-attributes-of-functions/v850-function-attributes
  declaring-attributes-of-functions/visium-function-attributes
  declaring-attributes-of-functions/x86-function-attributes
  declaring-attributes-of-functions/xstormy16-function-attributes