..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: parameters, precompiled headers

.. _pch-target:

Parameters for Precompiled Header Validity Checking
***************************************************

.. function:: void * TARGET_GET_PCH_VALIDITY (size_t *sz)

  .. hook-start:TARGET_GET_PCH_VALIDITY

  This hook returns a pointer to the data needed by
  ``TARGET_PCH_VALID_P`` and sets
  :samp:`*{sz}` to the size of the data in bytes.

.. hook-end

.. function:: const char * TARGET_PCH_VALID_P (const void *data, size_t sz)

  .. hook-start:TARGET_PCH_VALID_P

  This hook checks whether the options used to create a PCH file are
  compatible with the current settings.  It returns ``NULL``
  if so and a suitable error message if not.  Error messages will
  be presented to the user and must be localized using :samp:`_({msg})`.

  :samp:`{data}` is the data that was returned by ``TARGET_GET_PCH_VALIDITY``
  when the PCH file was created and :samp:`{sz}` is the size of that data in bytes.
  It's safe to assume that the data was created by the same version of the
  compiler, so no format checking is needed.

  The default definition of ``default_pch_valid_p`` should be
  suitable for most targets.

.. hook-end

.. function:: const char * TARGET_CHECK_PCH_TARGET_FLAGS (int pch_flags)

  .. hook-start:TARGET_CHECK_PCH_TARGET_FLAGS

  If this hook is nonnull, the default implementation of
  ``TARGET_PCH_VALID_P`` will use it to check for compatible values
  of ``target_flags``.  :samp:`{pch_flags}` specifies the value that
  ``target_flags`` had when the PCH file was created.  The return
  value is the same as for ``TARGET_PCH_VALID_P``.

.. hook-end

.. function:: void TARGET_PREPARE_PCH_SAVE (void)

  .. hook-start:TARGET_PREPARE_PCH_SAVE

  Called before writing out a PCH file.  If the target has some
  garbage-collected data that needs to be in a particular state on PCH loads,
  it can use this hook to enforce that state.  Very few targets need
  to do anything here.

.. hook-end