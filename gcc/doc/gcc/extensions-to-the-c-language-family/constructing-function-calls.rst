..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: constructing calls, forwarding calls

.. _constructing-calls:

Constructing Function Calls
***************************

Using the built-in functions described below, you can record
the arguments a function received, and call another function
with the same arguments, without knowing the number or types
of the arguments.

You can also record the return value of that function call,
and later return that value, without knowing what data type
the function tried to return (as long as your caller expects
that data type).

However, these built-in functions may interact badly with some
sophisticated features or other extensions of the language.  It
is, therefore, not recommended to use them outside very simple
functions acting as mere forwarders for their arguments.

.. function:: void * __builtin_apply_args ()

  This built-in function returns a pointer to data
  describing how to perform a call with the same arguments as are passed
  to the current function.

  The function saves the arg pointer register, structure value address,
  and all registers that might be used to pass arguments to a function
  into a block of memory allocated on the stack.  Then it returns the
  address of that block.

.. function:: void * __builtin_apply (void (*function)(), void *arguments, size_t size)

  This built-in function invokes :samp:`{function}`
  with a copy of the parameters described by :samp:`{arguments}`
  and :samp:`{size}`.

  The value of :samp:`{arguments}` should be the value returned by
  ``__builtin_apply_args``.  The argument :samp:`{size}` specifies the size
  of the stack argument data, in bytes.

  This function returns a pointer to data describing
  how to return whatever value is returned by :samp:`{function}`.  The data
  is saved in a block of memory allocated on the stack.

  It is not always simple to compute the proper value for :samp:`{size}`.  The
  value is used by ``__builtin_apply`` to compute the amount of data
  that should be pushed on the stack and copied from the incoming argument
  area.

.. function:: void __builtin_return (void *result)

  This built-in function returns the value described by :samp:`{result}` from
  the containing function.  You should specify, for :samp:`{result}`, a value
  returned by ``__builtin_apply``.

.. function:: __builtin_va_arg_pack ()

  This built-in function represents all anonymous arguments of an inline
  function.  It can be used only in inline functions that are always
  inlined, never compiled as a separate function, such as those using
  ``__attribute__ ((__always_inline__))`` or
  ``__attribute__ ((__gnu_inline__))`` extern inline functions.
  It must be only passed as last argument to some other function
  with variable arguments.  This is useful for writing small wrapper
  inlines for variable argument functions, when using preprocessor
  macros is undesirable.  For example:

  .. code-block:: c++

    extern int myprintf (FILE *f, const char *format, ...);
    extern inline __attribute__ ((__gnu_inline__)) int
    myprintf (FILE *f, const char *format, ...)
    {
      int r = fprintf (f, "myprintf: ");
      if (r < 0)
        return r;
      int s = fprintf (f, format, __builtin_va_arg_pack ());
      if (s < 0)
        return s;
      return r + s;
    }

.. function:: size_t __builtin_va_arg_pack_len ()

  This built-in function returns the number of anonymous arguments of
  an inline function.  It can be used only in inline functions that
  are always inlined, never compiled as a separate function, such
  as those using ``__attribute__ ((__always_inline__))`` or
  ``__attribute__ ((__gnu_inline__))`` extern inline functions.
  For example following does link- or run-time checking of open
  arguments for optimized code:

  .. code-block:: c++

    #ifdef __OPTIMIZE__
    extern inline __attribute__((__gnu_inline__)) int
    myopen (const char *path, int oflag, ...)
    {
      if (__builtin_va_arg_pack_len () > 1)
        warn_open_too_many_arguments ();

      if (__builtin_constant_p (oflag))
        {
          if ((oflag & O_CREAT) != 0 && __builtin_va_arg_pack_len () < 1)
            {
              warn_open_missing_mode ();
              return __open_2 (path, oflag);
            }
          return open (path, oflag, __builtin_va_arg_pack ());
        }

      if (__builtin_va_arg_pack_len () < 1)
        return __open_2 (path, oflag);

      return open (path, oflag, __builtin_va_arg_pack ());
    }
    #endif
