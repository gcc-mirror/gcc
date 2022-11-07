..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: __builtin_object_size, __builtin_dynamic_object_size, __builtin___memcpy_chk, __builtin___mempcpy_chk, __builtin___memmove_chk, __builtin___memset_chk, __builtin___strcpy_chk, __builtin___stpcpy_chk, __builtin___strncpy_chk, __builtin___strcat_chk, __builtin___strncat_chk, __builtin___sprintf_chk, __builtin___snprintf_chk, __builtin___vsprintf_chk, __builtin___vsnprintf_chk, __builtin___printf_chk, __builtin___vprintf_chk, __builtin___fprintf_chk, __builtin___vfprintf_chk

.. _object-size-checking:

Object Size Checking Built-in Functions
***************************************

GCC implements a limited buffer overflow protection mechanism that can
prevent some buffer overflow attacks by determining the sizes of objects
into which data is about to be written and preventing the writes when
the size isn't sufficient.  The built-in functions described below yield
the best results when used together and when optimization is enabled.
For example, to detect object sizes across function boundaries or to
follow pointer assignments through non-trivial control flow they rely
on various optimization passes enabled with :option:`-O2`.  However, to
a limited extent, they can be used without optimization as well.

.. function:: size_t __builtin_object_size (const void * ptr, int type)

  is a built-in construct that returns a constant number of bytes from
  :samp:`{ptr}` to the end of the object :samp:`{ptr}` pointer points to
  (if known at compile time).  To determine the sizes of dynamically allocated
  objects the function relies on the allocation functions called to obtain
  the storage to be declared with the ``alloc_size`` attribute (see :ref:`common-function-attributes`).  ``__builtin_object_size`` never evaluates
  its arguments for side effects.  If there are any side effects in them, it
  returns ``(size_t) -1`` for :samp:`{type}` 0 or 1 and ``(size_t) 0``
  for :samp:`{type}` 2 or 3.  If there are multiple objects :samp:`{ptr}` can
  point to and all of them are known at compile time, the returned number
  is the maximum of remaining byte counts in those objects if :samp:`{type}` & 2 is
  0 and minimum if nonzero.  If it is not possible to determine which objects
  :samp:`{ptr}` points to at compile time, ``__builtin_object_size`` should
  return ``(size_t) -1`` for :samp:`{type}` 0 or 1 and ``(size_t) 0``
  for :samp:`{type}` 2 or 3.

  :samp:`{type}` is an integer constant from 0 to 3.  If the least significant
  bit is clear, objects are whole variables, if it is set, a closest
  surrounding subobject is considered the object a pointer points to.
  The second bit determines if maximum or minimum of remaining bytes
  is computed.

  .. code-block:: c++

    struct V { char buf1[10]; int b; char buf2[10]; } var;
    char *p = &var.buf1[1], *q = &var.b;

    /* Here the object p points to is var.  */
    assert (__builtin_object_size (p, 0) == sizeof (var) - 1);
    /* The subobject p points to is var.buf1.  */
    assert (__builtin_object_size (p, 1) == sizeof (var.buf1) - 1);
    /* The object q points to is var.  */
    assert (__builtin_object_size (q, 0)
            == (char *) (&var + 1) - (char *) &var.b);
    /* The subobject q points to is var.b.  */
    assert (__builtin_object_size (q, 1) == sizeof (var.b));

.. function:: size_t __builtin_dynamic_object_size (const void * ptr, int type)

  is similar to ``__builtin_object_size`` in that it returns a number of bytes
  from :samp:`{ptr}` to the end of the object :samp:`{ptr}` pointer points to, except
  that the size returned may not be a constant.  This results in successful
  evaluation of object size estimates in a wider range of use cases and can be
  more precise than ``__builtin_object_size``, but it incurs a performance
  penalty since it may add a runtime overhead on size computation.  Semantics of
  :samp:`{type}` as well as return values in case it is not possible to determine
  which objects :samp:`{ptr}` points to at compile time are the same as in the case
  of ``__builtin_object_size``.

There are built-in functions added for many common string operation
functions, e.g., for ``memcpy`` ``__builtin___memcpy_chk``
built-in is provided.  This built-in has an additional last argument,
which is the number of bytes remaining in the object the :samp:`{dest}`
argument points to or ``(size_t) -1`` if the size is not known.

The built-in functions are optimized into the normal string functions
like ``memcpy`` if the last argument is ``(size_t) -1`` or if
it is known at compile time that the destination object will not
be overflowed.  If the compiler can determine at compile time that the
object will always be overflowed, it issues a warning.

The intended use can be e.g.

.. code-block:: c++

  #undef memcpy
  #define bos0(dest) __builtin_object_size (dest, 0)
  #define memcpy(dest, src, n) \
    __builtin___memcpy_chk (dest, src, n, bos0 (dest))

  char *volatile p;
  char buf[10];
  /* It is unknown what object p points to, so this is optimized
     into plain memcpy - no checking is possible.  */
  memcpy (p, "abcde", n);
  /* Destination is known and length too.  It is known at compile
     time there will be no overflow.  */
  memcpy (&buf[5], "abcde", 5);
  /* Destination is known, but the length is not known at compile time.
     This will result in __memcpy_chk call that can check for overflow
     at run time.  */
  memcpy (&buf[5], "abcde", n);
  /* Destination is known and it is known at compile time there will
     be overflow.  There will be a warning and __memcpy_chk call that
     will abort the program at run time.  */
  memcpy (&buf[6], "abcde", 5);

Such built-in functions are provided for ``memcpy``, ``mempcpy``,
``memmove``, ``memset``, ``strcpy``, ``stpcpy``, ``strncpy``,
``strcat`` and ``strncat``.

There are also checking built-in functions for formatted output functions.

.. code-block:: c++

  int __builtin___sprintf_chk (char *s, int flag, size_t os, const char *fmt, ...);
  int __builtin___snprintf_chk (char *s, size_t maxlen, int flag, size_t os,
                                const char *fmt, ...);
  int __builtin___vsprintf_chk (char *s, int flag, size_t os, const char *fmt,
                                va_list ap);
  int __builtin___vsnprintf_chk (char *s, size_t maxlen, int flag, size_t os,
                                 const char *fmt, va_list ap);

The added :samp:`{flag}` argument is passed unchanged to ``__sprintf_chk``
etc. functions and can contain implementation specific flags on what
additional security measures the checking function might take, such as
handling ``%n`` differently.

The :samp:`{os}` argument is the object size :samp:`{s}` points to, like in the
other built-in functions.  There is a small difference in the behavior
though, if :samp:`{os}` is ``(size_t) -1``, the built-in functions are
optimized into the non-checking functions only if :samp:`{flag}` is 0, otherwise
the checking function is called with :samp:`{os}` argument set to
``(size_t) -1``.

In addition to this, there are checking built-in functions
``__builtin___printf_chk``, ``__builtin___vprintf_chk``,
``__builtin___fprintf_chk`` and ``__builtin___vfprintf_chk``.
These have just one additional argument, :samp:`{flag}`, right before
format string :samp:`{fmt}`.  If the compiler is able to optimize them to
``fputc`` etc. functions, it does, otherwise the checking function
is called and the :samp:`{flag}` argument passed to it.