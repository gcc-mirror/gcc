..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: host hooks, host functions

.. _host-common:

Host Common
***********

Some things are just not portable, even between similar operating systems,
and are too difficult for autoconf to detect.  They get implemented using
hook functions in the file specified by the :samp:`{host_hook_obj}`
variable in :samp:`config.gcc`.

.. function:: void HOST_HOOKS_EXTRA_SIGNALS (void)

  This host hook is used to set up handling for extra signals.  The most
  common thing to do in this hook is to detect stack overflow.

.. function:: void * HOST_HOOKS_GT_PCH_GET_ADDRESS (size_t   size, int fd)

  This host hook returns the address of some space that is likely to be
  free in some subsequent invocation of the compiler.  We intend to load
  the PCH data at this address such that the data need not be relocated.
  The area should be able to hold :samp:`{size}` bytes.  If the host uses
  ``mmap``, :samp:`{fd}` is an open file descriptor that can be used for
  probing.

.. function:: int HOST_HOOKS_GT_PCH_USE_ADDRESS (void * address,   size_t size, int fd, size_t offset)

  This host hook is called when a PCH file is about to be loaded.
  We want to load :samp:`{size}` bytes from :samp:`{fd}` at :samp:`{offset}`
  into memory at :samp:`{address}`.  The given address will be the result of
  a previous invocation of ``HOST_HOOKS_GT_PCH_GET_ADDRESS``.
  Return -1 if we couldn't allocate :samp:`{size}` bytes at :samp:`{address}`.
  Return 0 if the memory is allocated but the data is not loaded.  Return 1
  if the hook has performed everything.

  If the implementation uses reserved address space, free any reserved
  space beyond :samp:`{size}`, regardless of the return value.  If no PCH will
  be loaded, this hook may be called with :samp:`{size}` zero, in which case
  all reserved address space should be freed.

  Do not try to handle values of :samp:`{address}` that could not have been
  returned by this executable; just return -1.  Such values usually
  indicate an out-of-date PCH file (built by some other GCC executable),
  and such a PCH file won't work.

.. function:: size_t HOST_HOOKS_GT_PCH_ALLOC_GRANULARITY (void);

  This host hook returns the alignment required for allocating virtual
  memory.  Usually this is the same as getpagesize, but on some hosts the
  alignment for reserving memory differs from the pagesize for committing
  memory.
