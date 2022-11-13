..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: incompatibilities of GCC, traditional

.. _incompatibilities:

Incompatibilities of GCC
************************

There are several noteworthy incompatibilities between GNU C and K&R
(non-ISO) versions of C.

.. index:: string constants, read-only strings, shared strings

* GCC normally makes string constants read-only.  If several
  identical-looking string constants are used, GCC stores only one
  copy of the string.

  .. index:: mktemp, and constant strings

  One consequence is that you cannot call ``mktemp`` with a string
  constant argument.  The function ``mktemp`` always alters the
  string its argument points to.

  .. index:: sscanf, and constant strings, fscanf, and constant strings, scanf, and constant strings

  Another consequence is that ``sscanf`` does not work on some very
  old systems when passed a string constant as its format control string
  or input.  This is because ``sscanf`` incorrectly tries to write
  into the string constant.  Likewise ``fscanf`` and ``scanf``.

  The solution to these problems is to change the program to use
  ``char`` -array variables with initialization strings for these
  purposes instead of string constants.

* ``-2147483648`` is positive.

  This is because 2147483648 cannot fit in the type ``int``, so
  (following the ISO C rules) its data type is ``unsigned long int``.
  Negating this value yields 2147483648 again.

* GCC does not substitute macro arguments when they appear inside of
  string constants.  For example, the following macro in GCC

  .. code-block:: c++

    #define foo(a) "a"

  will produce output ``"a"`` regardless of what the argument :samp:`{a}` is.

  .. index:: setjmp incompatibilities, longjmp incompatibilities

* When you use ``setjmp`` and ``longjmp``, the only automatic
  variables guaranteed to remain valid are those declared
  ``volatile``.  This is a consequence of automatic register
  allocation.  Consider this function:

  .. code-block:: c++

    jmp_buf j;

    foo ()
    {
      int a, b;

      a = fun1 ();
      if (setjmp (j))
        return a;

      a = fun2 ();
      /* longjmp (j) may occur in fun3. */
      return a + fun3 ();
    }

  Here ``a`` may or may not be restored to its first value when the
  ``longjmp`` occurs.  If ``a`` is allocated in a register, then
  its first value is restored; otherwise, it keeps the last value stored
  in it.

  .. index:: W

  If you use the :option:`-W` option with the :option:`-O` option, you will
  get a warning when GCC thinks such a problem might be possible.

* Programs that use preprocessing directives in the middle of macro
  arguments do not work with GCC.  For example, a program like this
  will not work:

  .. code-block:: c++

    foobar (
    #define luser
            hack)

  ISO C does not permit such a construct.

* K&R compilers allow comments to cross over an inclusion boundary
  (i.e. started in an include file and ended in the including file).

  .. index:: external declaration scope, scope of external declarations, declaration scope

* Declarations of external variables and functions within a block apply
  only to the block containing the declaration.  In other words, they
  have the same scope as any other declaration in the same place.

  In some other C compilers, an ``extern`` declaration affects all the
  rest of the file even if it happens within a block.

* In traditional C, you can combine ``long``, etc., with a typedef name,
  as shown here:

  .. code-block:: c++

    typedef int foo;
    typedef long foo bar;

  In ISO C, this is not allowed: ``long`` and other type modifiers
  require an explicit ``int``.

  .. index:: typedef names as function parameters

* PCC allows typedef names to be used as function parameters.

* Traditional C allows the following erroneous pair of declarations to
  appear together in a given scope:

  .. code-block:: c++

    typedef int foo;
    typedef foo foo;

* GCC treats all characters of identifiers as significant.  According to
  K&R-1 (2.2), 'No more than the first eight characters are significant,
  although more may be used.'.  Also according to K&R-1 (2.2), 'An
  identifier is a sequence of letters and digits; the first character must
  be a letter.  The underscore _ counts as a letter.', but GCC also
  allows dollar signs in identifiers.

  .. index:: whitespace

* PCC allows whitespace in the middle of compound assignment operators
  such as :samp:`+=`.  GCC, following the ISO standard, does not
  allow this.

  .. index:: apostrophes, '

* GCC complains about unterminated character constants inside of
  preprocessing conditionals that fail.  Some programs have English
  comments enclosed in conditionals that are guaranteed to fail; if these
  comments contain apostrophes, GCC will probably report an error.  For
  example, this code would produce an error:

  .. code-block:: c++

    #if 0
    You can't expect this to work.
    #endif

  The best solution to such a problem is to put the text into an actual
  C comment delimited by :samp:`/*...*/`.

* Many user programs contain the declaration :samp:`long time ();`.  In the
  past, the system header files on many systems did not actually declare
  ``time``, so it did not matter what type your program declared it to
  return.  But in systems with ISO C headers, ``time`` is declared to
  return ``time_t``, and if that is not the same as ``long``, then
  :samp:`long time ();` is erroneous.

  The solution is to change your program to use appropriate system headers
  (``<time.h>`` on systems with ISO C headers) and not to declare
  ``time`` if the system header files declare it, or failing that to
  use ``time_t`` as the return type of ``time``.

  .. index:: float as function value type

* When compiling functions that return ``float``, PCC converts it to
  a double.  GCC actually returns a ``float``.  If you are concerned
  with PCC compatibility, you should declare your functions to return
  ``double`` ; you might as well say what you mean.

  .. index:: structures, unions

* When compiling functions that return structures or unions, GCC
  output code normally uses a method different from that used on most
  versions of Unix.  As a result, code compiled with GCC cannot call
  a structure-returning function compiled with PCC, and vice versa.

  The method used by GCC is as follows: a structure or union which is
  1, 2, 4 or 8 bytes long is returned like a scalar.  A structure or union
  with any other size is stored into an address supplied by the caller
  (usually in a special, fixed register, but on some machines it is passed
  on the stack).  The target hook ``TARGET_STRUCT_VALUE_RTX``
  tells GCC where to pass this address.

  By contrast, PCC on most target machines returns structures and unions
  of any size by copying the data into an area of static storage, and then
  returning the address of that storage as if it were a pointer value.
  The caller must copy the data from that memory area to the place where
  the value is wanted.  GCC does not use this method because it is
  slower and nonreentrant.

  On some newer machines, PCC uses a reentrant convention for all
  structure and union returning.  GCC on most of these machines uses a
  compatible convention when returning structures and unions in memory,
  but still returns small structures and unions in registers.

  .. index:: fpcc-struct-return

  You can tell GCC to use a compatible convention for all structure and
  union returning with the option :option:`-fpcc-struct-return`.

  .. index:: preprocessing tokens, preprocessing numbers

* GCC complains about program fragments such as :samp:`0x74ae-0x4000`
  which appear to be two hexadecimal constants separated by the minus
  operator.  Actually, this string is a single :dfn:`preprocessing token`.
  Each such token must correspond to one token in C.  Since this does not,
  GCC prints an error message.  Although it may appear obvious that what
  is meant is an operator and two values, the ISO C standard specifically
  requires that this be treated as erroneous.

  A :dfn:`preprocessing token` is a :dfn:`preprocessing number` if it
  begins with a digit and is followed by letters, underscores, digits,
  periods and :samp:`e+`, :samp:`e-`, :samp:`E+`, :samp:`E-`, :samp:`p+`,
  :samp:`p-`, :samp:`P+`, or :samp:`P-` character sequences.  (In strict C90
  mode, the sequences :samp:`p+`, :samp:`p-`, :samp:`P+` and :samp:`P-` cannot
  appear in preprocessing numbers.)

  To make the above program fragment valid, place whitespace in front of
  the minus sign.  This whitespace will end the preprocessing number.