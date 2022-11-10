..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _static-analyzer-options:

Options That Control Static Analysis
************************************

.. option:: -fanalyzer

  This option enables an static analysis of program flow which looks
  for 'interesting' interprocedural paths through the
  code, and issues warnings for problems found on them.

  This analysis is much more expensive than other GCC warnings.

  Enabling this option effectively enables the following warnings:

  :option:`-Wanalyzer-allocation-size` |gol|
  :option:`-Wanalyzer-deref-before-check` |gol|
  :option:`-Wanalyzer-double-fclose` |gol|
  :option:`-Wanalyzer-double-free` |gol|
  :option:`-Wanalyzer-exposure-through-output-file` |gol|
  :option:`-Wanalyzer-exposure-through-uninit-copy` |gol|
  :option:`-Wanalyzer-fd-access-mode-mismatch` |gol|
  :option:`-Wanalyzer-fd-double-close` |gol|
  :option:`-Wanalyzer-fd-leak` |gol|
  :option:`-Wanalyzer-fd-use-after-close` |gol|
  :option:`-Wanalyzer-fd-use-without-check` |gol|
  :option:`-Wanalyzer-file-leak` |gol|
  :option:`-Wanalyzer-free-of-non-heap` |gol|
  :option:`-Wanalyzer-imprecise-fp-arithmetic` |gol|
  :option:`-Wanalyzer-jump-through-null` |gol|
  :option:`-Wanalyzer-malloc-leak` |gol|
  :option:`-Wanalyzer-mismatching-deallocation` |gol|
  :option:`-Wanalyzer-null-argument` |gol|
  :option:`-Wanalyzer-null-dereference` |gol|
  :option:`-Wanalyzer-out-of-bounds` |gol|
  :option:`-Wanalyzer-possible-null-argument` |gol|
  :option:`-Wanalyzer-possible-null-dereference` |gol|
  :option:`-Wanalyzer-putenv-of-auto-var` |gol|
  :option:`-Wanalyzer-shift-count-negative` |gol|
  :option:`-Wanalyzer-shift-count-overflow` |gol|
  :option:`-Wanalyzer-stale-setjmp-buffer` |gol|
  :option:`-Wanalyzer-unsafe-call-within-signal-handler` |gol|
  :option:`-Wanalyzer-use-after-free` |gol|
  :option:`-Wanalyzer-use-of-pointer-in-stale-stack-frame` |gol|
  :option:`-Wanalyzer-use-of-uninitialized-value` |gol|
  :option:`-Wanalyzer-va-arg-type-mismatch` |gol|
  :option:`-Wanalyzer-va-list-exhausted` |gol|
  :option:`-Wanalyzer-va-list-leak` |gol|
  :option:`-Wanalyzer-va-list-use-after-va-end` |gol|
  :option:`-Wanalyzer-write-to-const` |gol|
  :option:`-Wanalyzer-write-to-string-literal`

.. option:: -fno-analyzer

  Default setting; overrides :option:`-fanalyzer`.

.. option:: -Wanalyzer-too-complex

  If :option:`-fanalyzer` is enabled, the analyzer uses various heuristics
  to attempt to explore the control flow and data flow in the program,
  but these can be defeated by sufficiently complicated code.

  By default, the analysis silently stops if the code is too
  complicated for the analyzer to fully explore and it reaches an internal
  limit.  The :option:`-Wanalyzer-too-complex` option warns if this occurs.

.. option:: -Wno-analyzer-too-complex

  Default setting; overrides :option:`-Wanalyzer-too-complex`.

.. option:: -Wno-analyzer-allocation-size

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-allocation-size`
  to disable it.

  This diagnostic warns for paths through the code in which a pointer to
  a buffer is assigned to point at a buffer with a size that is not a
  multiple of ``sizeof (*pointer)``.

  See `CWE-131: Incorrect Calculation of Buffer Size <https://cwe.mitre.org/data/definitions/131.html>`_.

.. option:: -Wanalyzer-allocation-size

  Default setting; overrides :option:`-Wno-analyzer-allocation-size`.

.. option:: -Wno-analyzer-deref-before-check

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-deref-before-check`
  to disable it.

  This diagnostic warns for paths through the code in which a pointer
  is checked for ``NULL`` *after* it has already been
  dereferenced, suggesting that the pointer could have been NULL.
  Such cases suggest that the check for NULL is either redundant,
  or that it needs to be moved to before the pointer is dereferenced.

  This diagnostic also considers values passed to a function argument
  marked with ``__attribute__((nonnull))`` as requiring a non-NULL
  value, and thus will complain if such values are checked for ``NULL``
  after returning from such a function call.

  This diagnostic is unlikely to be reported when any level of optimization
  is enabled, as GCC's optimization logic will typically consider such
  checks for NULL as being redundant, and optimize them away before the
  analyzer "sees" them.  Hence optimization should be disabled when
  attempting to trigger this diagnostic.

.. option:: -Wanalyzer-deref-before-check

  Default setting; overrides :option:`-Wno-analyzer-deref-before-check`.

.. option:: -Wno-analyzer-double-fclose

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-double-fclose` to disable it.

  This diagnostic warns for paths through the code in which a ``FILE *``
  can have ``fclose`` called on it more than once.

  See `CWE-1341: Multiple Releases of Same Resource or Handle <https://cwe.mitre.org/data/definitions/1341.html>`_.

.. option:: -Wanalyzer-double-fclose

  Default setting; overrides :option:`-Wno-analyzer-double-fclose`.

.. option:: -Wno-analyzer-double-free

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-double-free` to disable it.

  This diagnostic warns for paths through the code in which a pointer
  can have a deallocator called on it more than once, either ``free``,
  or a deallocator referenced by attribute ``malloc``.

  See `CWE-415: Double Free <https://cwe.mitre.org/data/definitions/415.html>`_.

.. option:: -Wanalyzer-double-free

  Default setting; overrides :option:`-Wno-analyzer-double-free`.

.. option:: -Wno-analyzer-exposure-through-output-file

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-exposure-through-output-file`
  to disable it.

  This diagnostic warns for paths through the code in which a
  security-sensitive value is written to an output file
  (such as writing a password to a log file).

  See `CWE-532: Information Exposure Through Log Files <https://cwe.mitre.org/data/definitions/532.html>`_.

.. option:: -Wanalyzer-exposure-through-output-file

  Default setting; overrides :option:`-Wno-analyzer-exposure-through-output-file`.

.. option:: -Wanalyzer-exposure-through-uninit-copy

  This warning requires both :option:`-fanalyzer` and the use of a plugin
  to specify a function that copies across a 'trust boundary'.  Use
  :option:`-Wno-analyzer-exposure-through-uninit-copy` to disable it.

  This diagnostic warns for 'infoleaks' - paths through the code in which
  uninitialized values are copied across a security boundary
  (such as code within an OS kernel that copies a partially-initialized
  struct on the stack to user space).

  See `CWE-200: Exposure of Sensitive Information to an Unauthorized Actor <https://cwe.mitre.org/data/definitions/200.html>`_.

.. option:: -Wno-analyzer-exposure-through-uninit-copy

  Default setting; overrides :option:`-Wanalyzer-exposure-through-uninit-copy`.

.. option:: -Wno-analyzer-fd-access-mode-mismatch

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-fd-access-mode-mismatch`
  to disable it.

  This diagnostic warns for paths through code in which a
  ``read`` on a write-only file descriptor is attempted, or vice versa.

  This diagnostic also warns for code paths in a which a function with attribute
  ``fd_arg_read (N)`` is called with a file descriptor opened with
  ``O_WRONLY`` at referenced argument ``N`` or a function with attribute
  ``fd_arg_write (N)`` is called with a file descriptor opened with
  ``O_RDONLY`` at referenced argument :samp:`{N}`.

.. option:: -Wanalyzer-fd-access-mode-mismatch

  Default setting; overrides :option:`-Wno-analyzer-fd-access-mode-mismatch`.

.. option:: -Wno-analyzer-fd-double-close

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-fd-double-close`
  to disable it.

  This diagnostic warns for paths through code in which a
  file descriptor can be closed more than once.

  See `CWE-1341: Multiple Releases of Same Resource or Handle <https://cwe.mitre.org/data/definitions/1341.html>`_.

.. option:: -Wanalyzer-fd-double-close

  Default setting; overrides :option:`-Wno-analyzer-fd-double-close`.

.. option:: -Wno-analyzer-fd-leak

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-fd-leak`
  to disable it.

  This diagnostic warns for paths through code in which an
  open file descriptor is leaked.

  See `CWE-775: Missing Release of File Descriptor or Handle after Effective Lifetime <https://cwe.mitre.org/data/definitions/775.html>`_.

.. option:: -Wanalyzer-fd-leak

  Default setting; overrides :option:`-Wno-analyzer-fd-leak`.

.. option:: -Wno-analyzer-fd-use-after-close

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-fd-use-after-close`
  to disable it.

  This diagnostic warns for paths through code in which a
  read or write is called on a closed file descriptor.

  This diagnostic also warns for paths through code in which
  a function with attribute ``fd_arg (N)`` or ``fd_arg_read (N)``
  or ``fd_arg_write (N)`` is called with a closed file descriptor at
  referenced argument ``N``.

.. option:: -Wanalyzer-fd-use-after-close

  Default setting; overrides :option:`-Wno-analyzer-fd-use-after-close`.

.. option:: -Wno-analyzer-fd-use-without-check

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-fd-use-without-check`
  to disable it.

  This diagnostic warns for paths through code in which a
  file descriptor is used without being checked for validity.

  This diagnostic also warns for paths through code in which
  a function with attribute ``fd_arg (N)`` or ``fd_arg_read (N)``
  or ``fd_arg_write (N)`` is called with a file descriptor, at referenced
  argument ``N``, without being checked for validity.

.. option:: -Wanalyzer-fd-use-without-check

  Default setting; overrides :option:`-Wno-analyzer-fd-use-without-check`.

.. option:: -Wno-analyzer-file-leak

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-file-leak`
  to disable it.

  This diagnostic warns for paths through the code in which a
  ``<stdio.h>`` ``FILE *`` stream object is leaked.

  See `CWE-775: Missing Release of File Descriptor or Handle after Effective Lifetime <https://cwe.mitre.org/data/definitions/775.html>`_.

.. option:: -Wanalyzer-file-leak

  Default setting; overrides :option:`-Wno-analyzer-file-leak`.

.. option:: -Wno-analyzer-free-of-non-heap

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-free-of-non-heap`
  to disable it.

  This diagnostic warns for paths through the code in which ``free``
  is called on a non-heap pointer (e.g. an on-stack buffer, or a global).

  See `CWE-590: Free of Memory not on the Heap <https://cwe.mitre.org/data/definitions/590.html>`_.

.. option:: -Wanalyzer-free-of-non-heap

  Default setting; overrides :option:`-Wno-analyzer-free-of-non-heap`.

.. option:: -Wno-analyzer-imprecise-fp-arithmetic

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-imprecise-fp-arithmetic`
  to disable it.

  This diagnostic warns for paths through the code in which floating-point
  arithmetic is used in locations where precise computation is needed.  This
  diagnostic only warns on use of floating-point operands inside the
  calculation of an allocation size at the moment.

.. option:: -Wanalyzer-imprecise-fp-arithmetic

  Default setting; overrides :option:`-Wno-analyzer-imprecise-fp-arithmetic`.

.. option:: -Wno-analyzer-jump-through-null

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-jump-through-null`
  to disable it.

  This diagnostic warns for paths through the code in which a ``NULL``
  function pointer is called.

.. option:: -Wanalyzer-jump-through-null

  Default setting; overrides :option:`-Wno-analyzer-jump-through-null`.

.. option:: -Wno-analyzer-malloc-leak

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-malloc-leak`
  to disable it.

  This diagnostic warns for paths through the code in which a
  pointer allocated via an allocator is leaked: either ``malloc``,
  or a function marked with attribute ``malloc``.

  See `CWE-401: Missing Release of Memory after Effective Lifetime <https://cwe.mitre.org/data/definitions/401.html>`_.

.. option:: -Wanalyzer-malloc-leak

  Default setting; overrides :option:`-Wno-analyzer-malloc-leak`.

.. option:: -Wno-analyzer-mismatching-deallocation

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-mismatching-deallocation`
  to disable it.

  This diagnostic warns for paths through the code in which the
  wrong deallocation function is called on a pointer value, based on
  which function was used to allocate the pointer value.  The diagnostic
  will warn about mismatches between ``free``, scalar ``delete``
  and vector ``delete[]``, and those marked as allocator/deallocator
  pairs using attribute ``malloc``.

  See `CWE-762: Mismatched Memory Management Routines <https://cwe.mitre.org/data/definitions/762.html>`_.

.. option:: -Wanalyzer-mismatching-deallocation

  Default setting; overrides :option:`-Wno-analyzer-mismatching-deallocation`.

.. option:: -Wno-analyzer-out-of-bounds

  This warning requires :option:`-fanalyzer` to enable it; use
  :option:`-Wno-analyzer-out-of-bounds` to disable it.

  This diagnostic warns for path through the code in which a buffer is
  definitely read or written out-of-bounds.  The diagnostic applies for
  cases where the analyzer is able to determine a constant offset and for
  accesses past the end of a buffer, also a constant capacity.  Further,
  the diagnostic does limited checking for accesses past the end when the
  offset as well as the capacity is symbolic.

  See `CWE-119: Improper Restriction of Operations within the Bounds of a Memory Buffer <https://cwe.mitre.org/data/definitions/119.html>`_.

.. option:: -Wanalyzer-out-of-bounds

  Default setting; overrides :option:`-Wno-analyzer-out-of-bounds`.

.. option:: -Wno-analyzer-possible-null-argument

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-possible-null-argument` to disable it.

  This diagnostic warns for paths through the code in which a
  possibly-NULL value is passed to a function argument marked
  with ``__attribute__((nonnull))`` as requiring a non-NULL
  value.

  See `CWE-690: Unchecked Return Value to NULL Pointer Dereference <https://cwe.mitre.org/data/definitions/690.html>`_.

.. option:: -Wanalyzer-possible-null-argument

  Default setting; overrides :option:`-Wno-analyzer-possible-null-argument`.

.. option:: -Wno-analyzer-possible-null-dereference

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-possible-null-dereference` to disable it.

  This diagnostic warns for paths through the code in which a
  possibly-NULL value is dereferenced.

  See `CWE-690: Unchecked Return Value to NULL Pointer Dereference <https://cwe.mitre.org/data/definitions/690.html>`_.

.. option:: -Wanalyzer-possible-null-dereference

  Default setting; overrides :option:`-Wno-analyzer-possible-null-dereference`.

.. option:: -Wno-analyzer-null-argument

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-null-argument` to disable it.

  This diagnostic warns for paths through the code in which a
  value known to be NULL is passed to a function argument marked
  with ``__attribute__((nonnull))`` as requiring a non-NULL
  value.

  See `CWE-476: NULL Pointer Dereference <https://cwe.mitre.org/data/definitions/476.html>`_.

.. option:: -Wanalyzer-null-argument

  Default setting; overrides :option:`-Wno-analyzer-null-argument`.

.. option:: -Wno-analyzer-null-dereference

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-null-dereference` to disable it.

  This diagnostic warns for paths through the code in which a
  value known to be NULL is dereferenced.

  See `CWE-476: NULL Pointer Dereference <https://cwe.mitre.org/data/definitions/476.html>`_.

.. option:: -Wanalyzer-null-dereference

  Default setting; overrides :option:`-Wno-analyzer-null-dereference`.

.. option:: -Wno-analyzer-putenv-of-auto-var

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-putenv-of-auto-var` to disable it.

  This diagnostic warns for paths through the code in which a
  call to ``putenv`` is passed a pointer to an automatic variable
  or an on-stack buffer.

  See `POS34-C. Do not call putenv() with a pointer to an automatic variable as the argument <https://wiki.sei.cmu.edu/confluence/x/6NYxBQ>`_.

.. option:: -Wanalyzer-putenv-of-auto-var

  Default setting; overrides :option:`-Wno-analyzer-putenv-of-auto-var`.

.. option:: -Wno-analyzer-shift-count-negative

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-shift-count-negative` to disable it.

  This diagnostic warns for paths through the code in which a
  shift is attempted with a negative count.  It is analogous to
  the :option:`-Wshift-count-negative` diagnostic implemented in
  the C/C++ front ends, but is implemented based on analyzing
  interprocedural paths, rather than merely parsing the syntax tree.
  However, the analyzer does not prioritize detection of such paths, so
  false negatives are more likely relative to other warnings.

.. option:: -Wanalyzer-shift-count-negative

  Default setting; overrides :option:`-Wno-analyzer-shift-count-negative`.

.. option:: -Wno-analyzer-shift-count-overflow

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-shift-count-overflow` to disable it.

  This diagnostic warns for paths through the code in which a
  shift is attempted with a count greater than or equal to the
  precision of the operand's type.  It is analogous to
  the :option:`-Wshift-count-overflow` diagnostic implemented in
  the C/C++ front ends, but is implemented based on analyzing
  interprocedural paths, rather than merely parsing the syntax tree.
  However, the analyzer does not prioritize detection of such paths, so
  false negatives are more likely relative to other warnings.

.. option:: -Wanalyzer-shift-count-overflow

  Default setting; overrides :option:`-Wno-analyzer-shift-count-overflow`.

.. option:: -Wno-analyzer-stale-setjmp-buffer

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-stale-setjmp-buffer` to disable it.

  This diagnostic warns for paths through the code in which
  ``longjmp`` is called to rewind to a ``jmp_buf`` relating
  to a ``setjmp`` call in a function that has returned.

  When ``setjmp`` is called on a ``jmp_buf`` to record a rewind
  location, it records the stack frame.  The stack frame becomes invalid
  when the function containing the ``setjmp`` call returns.  Attempting
  to rewind to it via ``longjmp`` would reference a stack frame that
  no longer exists, and likely lead to a crash (or worse).

.. option:: -Wanalyzer-stale-setjmp-buffer

  Default setting; overrides :option:`-Wno-analyzer-stale-setjmp-buffer`.

.. option:: -Wno-analyzer-tainted-allocation-size

  This warning requires both :option:`-fanalyzer` and
  :option:`-fanalyzer-checker=taint` to enable it;
  use :option:`-Wno-analyzer-tainted-allocation-size` to disable it.

  This diagnostic warns for paths through the code in which a value
  that could be under an attacker's control is used as the size
  of an allocation without being sanitized, so that an attacker could
  inject an excessively large allocation and potentially cause a denial
  of service attack.

  See `CWE-789: Memory Allocation with Excessive Size Value <https://cwe.mitre.org/data/definitions/789.html>`_.

.. option:: -Wanalyzer-tainted-allocation-size

  Default setting; overrides :option:`-Wno-analyzer-tainted-allocation-size`.

.. option:: -Wno-analyzer-tainted-array-index

  This warning requires both :option:`-fanalyzer` and
  :option:`-fanalyzer-checker=taint` to enable it;
  use :option:`-Wno-analyzer-tainted-array-index` to disable it.

  This diagnostic warns for paths through the code in which a value
  that could be under an attacker's control is used as the index
  of an array access without being sanitized, so that an attacker
  could inject an out-of-bounds access.

  See `CWE-129: Improper Validation of Array Index <https://cwe.mitre.org/data/definitions/129.html>`_.

.. option:: -Wanalyzer-tainted-array-index

  Default setting; overrides :option:`-Wno-analyzer-tainted-array-index`.

.. option:: -Wno-analyzer-tainted-divisor

  This warning requires both :option:`-fanalyzer` and
  :option:`-fanalyzer-checker=taint` to enable it;
  use :option:`-Wno-analyzer-tainted-divisor` to disable it.

  This diagnostic warns for paths through the code in which a value
  that could be under an attacker's control is used as the divisor
  in a division or modulus operation without being sanitized, so that
  an attacker could inject a division-by-zero.

  See `CWE-369: Divide By Zero <https://cwe.mitre.org/data/definitions/369.html>`_.

.. option:: -Wanalyzer-tainted-divisor

  Default setting; overrides :option:`-Wno-analyzer-tainted-divisor`.

.. option:: -Wno-analyzer-tainted-offset

  This warning requires both :option:`-fanalyzer` and
  :option:`-fanalyzer-checker=taint` to enable it;
  use :option:`-Wno-analyzer-tainted-offset` to disable it.

  This diagnostic warns for paths through the code in which a value
  that could be under an attacker's control is used as a pointer offset
  without being sanitized, so that an attacker could inject an out-of-bounds
  access.

  See `CWE-823: Use of Out-of-range Pointer Offset <https://cwe.mitre.org/data/definitions/823.html>`_.

.. option:: -Wanalyzer-tainted-offset

  Default setting; overrides :option:`-Wno-analyzer-tainted-offset`.

.. option:: -Wno-analyzer-tainted-size

  This warning requires both :option:`-fanalyzer` and
  :option:`-fanalyzer-checker=taint` to enable it;
  use :option:`-Wno-analyzer-tainted-size` to disable it.

  This diagnostic warns for paths through the code in which a value
  that could be under an attacker's control is used as the size of
  an operation such as ``memset`` without being sanitized, so that an
  attacker could inject an out-of-bounds access.

  See `CWE-129: Improper Validation of Array Index <https://cwe.mitre.org/data/definitions/129.html>`_.

.. option:: -Wanalyzer-tainted-size

  Default setting; overrides :option:`-Wno-analyzer-tainted-size`.

.. option:: -Wno-analyzer-unsafe-call-within-signal-handler

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-unsafe-call-within-signal-handler` to disable it.

  This diagnostic warns for paths through the code in which a
  function known to be async-signal-unsafe (such as ``fprintf``) is
  called from a signal handler.

  See `CWE-479: Signal Handler Use of a Non-reentrant Function <https://cwe.mitre.org/data/definitions/479.html>`_.

.. option:: -Wanalyzer-unsafe-call-within-signal-handler

  Default setting; overrides :option:`-Wno-analyzer-unsafe-call-within-signal-handler`.

.. option:: -Wno-analyzer-use-after-free

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-use-after-free` to disable it.

  This diagnostic warns for paths through the code in which a
  pointer is used after a deallocator is called on it: either ``free``,
  or a deallocator referenced by attribute ``malloc``.

  See `CWE-416: Use After Free <https://cwe.mitre.org/data/definitions/416.html>`_.

.. option:: -Wanalyzer-use-after-free

  Default setting; overrides :option:`-Wno-analyzer-use-after-free`.

.. option:: -Wno-analyzer-use-of-pointer-in-stale-stack-frame

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-use-of-pointer-in-stale-stack-frame`
  to disable it.

  This diagnostic warns for paths through the code in which a pointer
  is dereferenced that points to a variable in a stale stack frame.

.. option:: -Wanalyzer-use-of-pointer-in-stale-stack-frame

  Default setting; overrides :option:`-Wno-analyzer-use-of-pointer-in-stale-stack-frame`.

.. option:: -Wno-analyzer-va-arg-type-mismatch

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-va-arg-type-mismatch`
  to disable it.

  This diagnostic warns for interprocedural paths through the code for which
  the analyzer detects an attempt to use ``va_arg`` to extract a value
  passed to a variadic call, but uses a type that does not match that of
  the expression passed to the call.

  See `CWE-686: Function Call With Incorrect Argument Type <https://cwe.mitre.org/data/definitions/686.html>`_.

.. option:: -Wanalyzer-va-arg-type-mismatch

  Default setting; overrides :option:`-Wno-analyzer-va-arg-type-mismatch`.

.. option:: -Wno-analyzer-va-list-exhausted

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-va-list-exhausted`
  to disable it.

  This diagnostic warns for interprocedural paths through the code for which
  the analyzer detects an attempt to use ``va_arg`` to access the next
  value passed to a variadic call, but all of the values in the
  ``va_list`` have already been consumed.

  See `CWE-685: Function Call With Incorrect Number of Arguments <https://cwe.mitre.org/data/definitions/685.html>`_.

.. option:: -Wanalyzer-va-list-exhausted

  Default setting; overrides :option:`-Wno-analyzer-va-list-exhausted`.

.. option:: -Wno-analyzer-va-list-leak

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-va-list-leak`
  to disable it.

  This diagnostic warns for interprocedural paths through the code for which
  the analyzer detects that ``va_start`` or ``va_copy`` has been called
  on a ``va_list`` without a corresponding call to ``va_end``.

.. option:: -Wanalyzer-va-list-leak

  Default setting; overrides :option:`-Wno-analyzer-va-list-leak`.

.. option:: -Wno-analyzer-va-list-use-after-va-end

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-va-list-use-after-va-end`
  to disable it.

  This diagnostic warns for interprocedural paths through the code for which
  the analyzer detects an attempt to use a ``va_list``  after
  ``va_end`` has been called on it.
  ``va_list``.

.. option:: -Wanalyzer-va-list-use-after-va-end

  Default setting; overrides :option:`-Wno-analyzer-va-list-use-after-va-end`.

.. option:: -Wno-analyzer-write-to-const

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-write-to-const`
  to disable it.

  This diagnostic warns for paths through the code in which the analyzer
  detects an attempt to write through a pointer to a ``const`` object.
  However, the analyzer does not prioritize detection of such paths, so
  false negatives are more likely relative to other warnings.

.. option:: -Wanalyzer-write-to-const

  Default setting; overrides :option:`-Wno-analyzer-write-to-const`.

.. option:: -Wno-analyzer-write-to-string-literal

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-write-to-string-literal`
  to disable it.

  This diagnostic warns for paths through the code in which the analyzer
  detects an attempt to write through a pointer to a string literal.
  However, the analyzer does not prioritize detection of such paths, so
  false negatives are more likely relative to other warnings.

.. option:: -Wanalyzer-write-to-string-literal

  Default setting; overrides :option:`-Wno-analyzer-write-to-string-literal`.

.. option:: -Wno-analyzer-use-of-uninitialized-value

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-use-of-uninitialized-value` to disable it.

  This diagnostic warns for paths through the code in which an uninitialized
  value is used.

  See `CWE-457: Use of Uninitialized Variable <https://cwe.mitre.org/data/definitions/457.html>`_.

.. option:: -Wanalyzer-use-of-uninitialized-value

  Default setting; overrides :option:`-Wno-analyzer-use-of-uninitialized-value`.

The analyzer has hardcoded knowledge about the behavior of the following
memory-management functions:

* ``alloca``
* The built-in functions ``__builtin_alloc``,
  ``__builtin_alloc_with_align``, ``__builtin_calloc``,
  ``__builtin_free``, ``__builtin_malloc``, ``__builtin_memcpy``,
  ``__builtin_memcpy_chk``, ``__builtin_memset``,
  ``__builtin_memset_chk``, ``__builtin_realloc``,
  ``__builtin_stack_restore``, and ``__builtin_stack_save``
* ``calloc``
* ``free``
* ``malloc``
* ``memset``
* ``operator delete``
* ``operator delete []``
* ``operator new``
* ``operator new []``
* ``realloc``
* ``strdup``
* ``strndup``

of the following functions for working with file descriptors:

* ``open``
* ``close``
* ``creat``
* ``dup``, ``dup2`` and ``dup3``
* ``pipe`` and ``pipe2``
* ``read``
* ``write``

of the following functions for working with ``<stdio.h>`` streams:

* The built-in functions ``__builtin_fprintf``,
  ``__builtin_fprintf_unlocked``, ``__builtin_fputc``,
  ``__builtin_fputc_unlocked``, ``__builtin_fputs``,
  ``__builtin_fputs_unlocked``, ``__builtin_fwrite``,
  ``__builtin_fwrite_unlocked``, ``__builtin_printf``,
  ``__builtin_printf_unlocked``, ``__builtin_putc``,
  ``__builtin_putchar``, ``__builtin_putchar_unlocked``,
  ``__builtin_putc_unlocked``, ``__builtin_puts``,
  ``__builtin_puts_unlocked``, ``__builtin_vfprintf``, and
  ``__builtin_vprintf``
* ``fopen``
* ``fclose``
* ``fgets``
* ``fgets_unlocked``
* ``fread``
* ``getchar``
* ``fprintf``
* ``printf``
* ``fwrite``

and of the following functions:

* The built-in functions ``__builtin_expect``,
  ``__builtin_expect_with_probability``, ``__builtin_strchr``,
  ``__builtin_strcpy``, ``__builtin_strcpy_chk``,
  ``__builtin_strlen``, ``__builtin_va_copy``, and
  ``__builtin_va_start``

* The GNU extensions ``error`` and ``error_at_line``

* ``getpass``
* ``longjmp``
* ``putenv``
* ``setjmp``
* ``siglongjmp``
* ``signal``
* ``sigsetjmp``
* ``strchr``
* ``strlen``

In addition, various functions with an ``__analyzer_`` prefix have
special meaning to the analyzer, described in the GCC Internals manual.

Pertinent parameters for controlling the exploration are:

:option:`--param` :gcc-param:`analyzer-bb-explosion-factor`:samp:`={value}`,
:option:`--param` :gcc-param:`analyzer-max-enodes-per-program-point`:samp:`={value}`,
:option:`--param` :gcc-param:`analyzer-max-recursion-depth`:samp:`={value}` and
:option:`--param` :gcc-param:`analyzer-min-snodes-for-call-summary`:samp:`={value}`.

The following options control the analyzer.

.. option:: -fanalyzer-call-summaries

  Simplify interprocedural analysis by computing the effect of certain calls,
  rather than exploring all paths through the function from callsite to each
  possible return.

  If enabled, call summaries are only used for functions with more than one
  call site, and that are sufficiently complicated (as per
  :option:`--param` :gcc-param:`analyzer-min-snodes-for-call-summary`:samp:`={value}`).

.. option:: -fno-analyzer-call-summaries

  Default setting; overrides :option:`-fanalyzer-call-summaries`.

.. option:: -fanalyzer-checker={name}

  Restrict the analyzer to run just the named checker, and enable it.

  Some checkers are disabled by default (even with :option:`-fanalyzer`),
  such as the ``taint`` checker that implements
  :option:`-Wanalyzer-tainted-array-index`, and this option is required
  to enable them.

  .. note::

    Currently, :option:`-fanalyzer-checker=taint` disables the
    following warnings from :option:`-fanalyzer` :

    :option:`-Wanalyzer-deref-before-check` |gol|
    :option:`-Wanalyzer-double-fclose` |gol|
    :option:`-Wanalyzer-double-free`  |gol|
    :option:`-Wanalyzer-exposure-through-output-file`  |gol|
    :option:`-Wanalyzer-fd-access-mode-mismatch`  |gol|
    :option:`-Wanalyzer-fd-double-close`  |gol|
    :option:`-Wanalyzer-fd-leak`  |gol|
    :option:`-Wanalyzer-fd-use-after-close`  |gol|
    :option:`-Wanalyzer-fd-use-without-check`  |gol|
    :option:`-Wanalyzer-file-leak`  |gol|
    :option:`-Wanalyzer-free-of-non-heap`  |gol|
    :option:`-Wanalyzer-malloc-leak`  |gol|
    :option:`-Wanalyzer-mismatching-deallocation`  |gol|
    :option:`-Wanalyzer-null-argument`  |gol|
    :option:`-Wanalyzer-null-dereference`  |gol|
    :option:`-Wanalyzer-possible-null-argument`  |gol|
    :option:`-Wanalyzer-possible-null-dereference`  |gol|
    :option:`-Wanalyzer-unsafe-call-within-signal-handler` |gol|
    :option:`-Wanalyzer-use-after-free`  |gol|
    :option:`-Wanalyzer-va-list-leak`  |gol|
    :option:`-Wanalyzer-va-list-use-after-va-end`

.. option:: -fno-analyzer-feasibility

  This option is intended for analyzer developers.

  By default the analyzer verifies that there is a feasible control flow path
  for each diagnostic it emits: that the conditions that hold are not mutually
  exclusive.  Diagnostics for which no feasible path can be found are rejected.
  This filtering can be suppressed with :option:`-fno-analyzer-feasibility`, for
  debugging issues in this code.

.. option:: -fanalyzer-feasibility

  Default setting; overrides :option:`-fno-analyzer-feasibility`.

.. option:: -fanalyzer-fine-grained

  This option is intended for analyzer developers.

  Internally the analyzer builds an 'exploded graph' that combines
  control flow graphs with data flow information.

  By default, an edge in this graph can contain the effects of a run
  of multiple statements within a basic block.  With
  :option:`-fanalyzer-fine-grained`, each statement gets its own edge.

.. option:: -fno-analyzer-fine-grained

  Default setting; overrides :option:`-fanalyzer-fine-grained`.

.. option:: -fanalyzer-show-duplicate-count

  This option is intended for analyzer developers: if multiple diagnostics
  have been detected as being duplicates of each other, it emits a note when
  reporting the best diagnostic, giving the number of additional diagnostics
  that were suppressed by the deduplication logic.

.. option:: -fno-analyzer-show-duplicate-count

  Default setting; overrides :option:`-fanalyzer-show-duplicate-count`.

.. option:: -fno-analyzer-state-merge

  This option is intended for analyzer developers.

  By default the analyzer attempts to simplify analysis by merging
  sufficiently similar states at each program point as it builds its
  'exploded graph'.  With :option:`-fno-analyzer-state-merge` this
  merging can be suppressed, for debugging state-handling issues.

.. option:: -fanalyzer-state-merge

  Default setting; overrides :option:`-fno-analyzer-state-merge`.

.. option:: -fno-analyzer-state-purge

  This option is intended for analyzer developers.

  By default the analyzer attempts to simplify analysis by purging
  aspects of state at a program point that appear to no longer be relevant
  e.g. the values of locals that aren't accessed later in the function
  and which aren't relevant to leak analysis.

  With :option:`-fno-analyzer-state-purge` this purging of state can
  be suppressed, for debugging state-handling issues.

.. option:: -fanalyzer-state-purge

  Default setting; overrides :option:`-fno-analyzer-state-purge`.

.. option:: -fanalyzer-transitivity

  This option enables transitivity of constraints within the analyzer.

.. option:: -fno-analyzer-transitivity

  Default setting; overrides :option:`-fanalyzer-transitivity`.

.. option:: -fno-analyzer-undo-inlining

  This option is intended for analyzer developers.

  :option:`-fanalyzer` runs relatively late compared to other code analysis
  tools, and some optimizations have already been applied to the code.  In
  particular function inlining may have occurred, leading to the
  interprocedural execution paths emitted by the analyzer containing
  function frames that don't correspond to those in the original source
  code.

  By default the analyzer attempts to reconstruct the original function
  frames, and to emit events showing the inlined calls.

  With :option:`-fno-analyzer-undo-inlining` this attempt to reconstruct
  the original frame information can be be disabled, which may be of help
  when debugging issues in the analyzer.

.. option:: -fanalyzer-undo-inlining

  Default setting; overrides :option:`-fno-analyzer-undo-inlining`.

.. option:: -fanalyzer-verbose-edges

  This option is intended for analyzer developers.  It enables more
  verbose, lower-level detail in the descriptions of control flow
  within diagnostic paths.

.. option:: -fanalyzer-verbose-state-changes

  This option is intended for analyzer developers.  It enables more
  verbose, lower-level detail in the descriptions of events relating
  to state machines within diagnostic paths.

.. option:: -fanalyzer-verbosity={level}

  This option controls the complexity of the control flow paths that are
  emitted for analyzer diagnostics.

  The :samp:`{level}` can be one of:

  :samp:`0`
    At this level, interprocedural call and return events are displayed,
    along with the most pertinent state-change events relating to
    a diagnostic.  For example, for a double- ``free`` diagnostic,
    both calls to ``free`` will be shown.

  :samp:`1`
    As per the previous level, but also show events for the entry
    to each function.

  :samp:`2`
    As per the previous level, but also show events relating to
    control flow that are significant to triggering the issue
    (e.g. 'true path taken' at a conditional).

    This level is the default.

  :samp:`3`
    As per the previous level, but show all control flow events, not
    just significant ones.

  :samp:`4`
    This level is intended for analyzer developers; it adds various
    other events intended for debugging the analyzer.

.. option:: -fdump-analyzer

  Dump internal details about what the analyzer is doing to
  :samp:`{file}.analyzer.txt`.
  This option is overridden by :option:`-fdump-analyzer-stderr`.

.. option:: -fdump-analyzer-stderr

  Dump internal details about what the analyzer is doing to stderr.
  This option overrides :option:`-fdump-analyzer`.

.. option:: -fdump-analyzer-callgraph

  Dump a representation of the call graph suitable for viewing with
  GraphViz to :samp:`{file}.callgraph.dot`.

.. option:: -fdump-analyzer-exploded-graph

  Dump a representation of the 'exploded graph' suitable for viewing with
  GraphViz to :samp:`{file}.eg.dot`.
  Nodes are color-coded based on state-machine states to emphasize
  state changes.

.. option:: -fdump-analyzer-exploded-nodes

  Emit diagnostics showing where nodes in the 'exploded graph' are
  in relation to the program source.

.. option:: -fdump-analyzer-exploded-nodes-2

  Dump a textual representation of the 'exploded graph' to
  :samp:`{file}.eg.txt`.

.. option:: -fdump-analyzer-exploded-nodes-3

  Dump a textual representation of the 'exploded graph' to
  one dump file per node, to :samp:`{file}.eg-{id}.txt`.
  This is typically a large number of dump files.

.. option:: -fdump-analyzer-exploded-paths

  Dump a textual representation of the 'exploded path' for each
  diagnostic to :samp:`{file}.{idx}.{kind}.epath.txt`.

.. option:: -fdump-analyzer-feasibility

  Dump internal details about the analyzer's search for feasible paths.
  The details are written in a form suitable for viewing with GraphViz
  to filenames of the form :samp:`{file}.*.fg.dot`,
  :samp:`{file}.*.tg.dot`, and :samp:`{file}.*.fpath.txt`.

.. option:: -fdump-analyzer-json

  Dump a compressed JSON representation of analyzer internals to
  :samp:`{file}.analyzer.json.gz`.  The precise format is subject
  to change.

.. option:: -fdump-analyzer-state-purge

  As per :option:`-fdump-analyzer-supergraph`, dump a representation of the
  'supergraph' suitable for viewing with GraphViz, but annotate the
  graph with information on what state will be purged at each node.
  The graph is written to :samp:`{file}.state-purge.dot`.

.. option:: -fdump-analyzer-supergraph

  Dump representations of the 'supergraph' suitable for viewing with
  GraphViz to :samp:`{file}.supergraph.dot` and to
  :samp:`{file}.supergraph-eg.dot`.  These show all of the
  control flow graphs in the program, with interprocedural edges for
  calls and returns.  The second dump contains annotations showing nodes
  in the 'exploded graph' and diagnostics associated with them.

.. option:: -fdump-analyzer-untracked

  Emit custom warnings with internal details intended for analyzer developers.
