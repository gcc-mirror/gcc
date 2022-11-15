..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: RTL object types, RTL integers, RTL strings, RTL vectors, RTL expression, RTX (See RTL)

.. _rtl-objects:

RTL Object Types
****************

RTL uses five kinds of objects: expressions, integers, wide integers,
strings and vectors.  Expressions are the most important ones.  An RTL
expression ('RTX', for short) is a C structure, but it is usually
referred to with a pointer; a type that is given the typedef name
``rtx``.

An integer is simply an ``int`` ; their written form uses decimal
digits.  A wide integer is an integral object whose type is
``HOST_WIDE_INT`` ; their written form uses decimal digits.

A string is a sequence of characters.  In core it is represented as a
``char *`` in usual C fashion, and it is written in C syntax as well.
However, strings in RTL may never be null.  If you write an empty string in
a machine description, it is represented in core as a null pointer rather
than as a pointer to a null character.  In certain contexts, these null
pointers instead of strings are valid.  Within RTL code, strings are most
commonly found inside ``symbol_ref`` expressions, but they appear in
other contexts in the RTL expressions that make up machine descriptions.

In a machine description, strings are normally written with double
quotes, as you would in C.  However, strings in machine descriptions may
extend over many lines, which is invalid C, and adjacent string
constants are not concatenated as they are in C.  Any string constant
may be surrounded with a single set of parentheses.  Sometimes this
makes the machine description easier to read.

There is also a special syntax for strings, which can be useful when C
code is embedded in a machine description.  Wherever a string can
appear, it is also valid to write a C-style brace block.  The entire
brace block, including the outermost pair of braces, is considered to be
the string constant.  Double quote characters inside the braces are not
special.  Therefore, if you write string constants in the C code, you
need not escape each quote character with a backslash.

A vector contains an arbitrary number of pointers to expressions.  The
number of elements in the vector is explicitly present in the vector.
The written form of a vector consists of square brackets
(:samp:`[...]`) surrounding the elements, in sequence and with
whitespace separating them.  Vectors of length zero are not created;
null pointers are used instead.

.. index:: expression codes, codes, RTL expression, GET_CODE, PUT_CODE

Expressions are classified by :dfn:`expression codes` (also called RTX
codes).  The expression code is a name defined in :samp:`rtl.def`, which is
also (in uppercase) a C enumeration constant.  The possible expression
codes and their meanings are machine-independent.  The code of an RTX can
be extracted with the macro ``GET_CODE (x)`` and altered with
``PUT_CODE (x, newcode)``.

The expression code determines how many operands the expression contains,
and what kinds of objects they are.  In RTL, unlike Lisp, you cannot tell
by looking at an operand what kind of object it is.  Instead, you must know
from its context---from the expression code of the containing expression.
For example, in an expression of code ``subreg``, the first operand is
to be regarded as an expression and the second operand as a polynomial
integer.  In an expression of code ``plus``, there are two operands,
both of which are to be regarded as expressions.  In a ``symbol_ref``
expression, there is one operand, which is to be regarded as a string.

Expressions are written as parentheses containing the name of the
expression type, its flags and machine mode if any, and then the operands
of the expression (separated by spaces).

Expression code names in the :samp:`md` file are written in lowercase,
but when they appear in C code they are written in uppercase.  In this
manual, they are shown as follows: ``const_int``.

.. index:: (nil), nil

In a few contexts a null pointer is valid where an expression is normally
wanted.  The written form of this is ``(nil)``.
