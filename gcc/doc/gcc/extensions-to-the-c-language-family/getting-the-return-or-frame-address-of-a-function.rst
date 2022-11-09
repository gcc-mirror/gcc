..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _return-address:

Getting the Return or Frame Address of a Function
*************************************************

These functions may be used to get information about the callers of a
function.

.. function:: void * __builtin_return_address (unsigned int level)

  This function returns the return address of the current function, or of
  one of its callers.  The :samp:`{level}` argument is number of frames to
  scan up the call stack.  A value of ``0`` yields the return address
  of the current function, a value of ``1`` yields the return address
  of the caller of the current function, and so forth.  When inlining
  the expected behavior is that the function returns the address of
  the function that is returned to.  To work around this behavior use
  the :fn-attr:`noinline` function attribute.

  The :samp:`{level}` argument must be a constant integer.

  On some machines it may be impossible to determine the return address of
  any function other than the current one; in such cases, or when the top
  of the stack has been reached, this function returns an unspecified
  value.  In addition, ``__builtin_frame_address`` may be used
  to determine if the top of the stack has been reached.

  Additional post-processing of the returned value may be needed, see
  ``__builtin_extract_return_addr``.

  The stored representation of the return address in memory may be different
  from the address returned by ``__builtin_return_address``.  For example,
  on AArch64 the stored address may be mangled with return address signing
  whereas the address returned by ``__builtin_return_address`` is not.

  Calling this function with a nonzero argument can have unpredictable
  effects, including crashing the calling program.  As a result, calls
  that are considered unsafe are diagnosed when the :option:`-Wframe-address`
  option is in effect.  Such calls should only be made in debugging
  situations.

  On targets where code addresses are representable as ``void *``,

  .. code-block:: c++

    void *addr = __builtin_extract_return_addr (__builtin_return_address (0));

  gives the code address where the current function would return.  For example,
  such an address may be used with ``dladdr`` or other interfaces that work
  with code addresses.

.. function:: void * __builtin_extract_return_addr (void *addr)

  The address as returned by ``__builtin_return_address`` may have to be fed
  through this function to get the actual encoded address.  For example, on the
  31-bit S/390 platform the highest bit has to be masked out, or on SPARC
  platforms an offset has to be added for the true next instruction to be
  executed.

  If no fixup is needed, this function simply passes through :samp:`{addr}`.

.. function:: void * __builtin_frob_return_addr (void *addr)

  This function does the reverse of ``__builtin_extract_return_addr``.

.. function:: void * __builtin_frame_address (unsigned int level)

  This function is similar to ``__builtin_return_address``, but it
  returns the address of the function frame rather than the return address
  of the function.  Calling ``__builtin_frame_address`` with a value of
  ``0`` yields the frame address of the current function, a value of
  ``1`` yields the frame address of the caller of the current function,
  and so forth.

  The frame is the area on the stack that holds local variables and saved
  registers.  The frame address is normally the address of the first word
  pushed on to the stack by the function.  However, the exact definition
  depends upon the processor and the calling convention.  If the processor
  has a dedicated frame pointer register, and the function has a frame,
  then ``__builtin_frame_address`` returns the value of the frame
  pointer register.

  On some machines it may be impossible to determine the frame address of
  any function other than the current one; in such cases, or when the top
  of the stack has been reached, this function returns ``0`` if
  the first frame pointer is properly initialized by the startup code.

  Calling this function with a nonzero argument can have unpredictable
  effects, including crashing the calling program.  As a result, calls
  that are considered unsafe are diagnosed when the :option:`-Wframe-address`
  option is in effect.  Such calls should only be made in debugging
  situations.
