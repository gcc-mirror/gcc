..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _aarch64-function-attributes:

AArch64 Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^

The following target-specific function attributes are available for the
AArch64 target.  For the most part, these options mirror the behavior of
similar command-line options (see :ref:`aarch64-options`), but on a
per-function basis.

.. index:: general-regs-only function attribute, AArch64

.. aarch64-fn-attr:: general-regs-only

  Indicates that no floating-point or Advanced SIMD registers should be
  used when generating code for this function.  If the function explicitly
  uses floating-point code, then the compiler gives an error.  This is
  the same behavior as that of the command-line option
  :option:`-mgeneral-regs-only`.

.. index:: fix-cortex-a53-835769 function attribute, AArch64

.. aarch64-fn-attr:: fix-cortex-a53-835769

  Indicates that the workaround for the Cortex-A53 erratum 835769 should be
  applied to this function.  To explicitly disable the workaround for this
  function specify the negated form: ``no-fix-cortex-a53-835769``.
  This corresponds to the behavior of the command line options
  :option:`-mfix-cortex-a53-835769` and :option:`-mno-fix-cortex-a53-835769`.

.. index:: cmodel= function attribute, AArch64

.. aarch64-fn-attr:: cmodel=

  Indicates that code should be generated for a particular code model for
  this function.  The behavior and permissible arguments are the same as
  for the command line option :option:`-mcmodel=`.

.. index:: strict-align function attribute, AArch64

.. aarch64-fn-attr:: strict-align, no-strict-align

  :aarch64-fn-attr:`strict-align` indicates that the compiler should not assume that unaligned
  memory references are handled by the system.  To allow the compiler to assume
  that aligned memory references are handled by the system, the inverse attribute
  ``no-strict-align`` can be specified.  The behavior is same as for the
  command-line option :option:`-mstrict-align` and :option:`-mno-strict-align`.

.. index:: omit-leaf-frame-pointer function attribute, AArch64

.. aarch64-fn-attr:: omit-leaf-frame-pointer

  Indicates that the frame pointer should be omitted for a leaf function call.
  To keep the frame pointer, the inverse attribute
  ``no-omit-leaf-frame-pointer`` can be specified.  These attributes have
  the same behavior as the command-line options :option:`-momit-leaf-frame-pointer`
  and :option:`-mno-omit-leaf-frame-pointer`.

.. index:: tls-dialect= function attribute, AArch64

.. aarch64-fn-attr:: tls-dialect=

  Specifies the TLS dialect to use for this function.  The behavior and
  permissible arguments are the same as for the command-line option
  :option:`-mtls-dialect=`.

.. index:: arch= function attribute, AArch64

.. aarch64-fn-attr:: arch=

  Specifies the architecture version and architectural extensions to use
  for this function.  The behavior and permissible arguments are the same as
  for the :option:`-march=` command-line option.

.. index:: tune= function attribute, AArch64

.. aarch64-fn-attr:: tune=

  Specifies the core for which to tune the performance of this function.
  The behavior and permissible arguments are the same as for the :option:`-mtune=`
  command-line option.

.. index:: cpu= function attribute, AArch64

.. aarch64-fn-attr:: cpu=

  Specifies the core for which to tune the performance of this function and also
  whose architectural features to use.  The behavior and valid arguments are the
  same as for the :option:`-mcpu=` command-line option.

.. index:: sign-return-address function attribute, AArch64

.. aarch64-fn-attr:: sign-return-address

  Select the function scope on which return address signing will be applied.  The
  behavior and permissible arguments are the same as for the command-line option
  :option:`-msign-return-address=`.  The default value is ``none``.  This
  attribute is deprecated.  The :gcc-attr:`branch-protection` attribute should
  be used instead.

.. index:: branch-protection function attribute, AArch64

.. aarch64-fn-attr:: branch-protection

  Select the function scope on which branch protection will be applied.  The
  behavior and permissible arguments are the same as for the command-line option
  :option:`-mbranch-protection=`.  The default value is ``none``.

.. index:: outline-atomics function attribute, AArch64

.. aarch64-fn-attr:: outline-atomics

  Enable or disable calls to out-of-line helpers to implement atomic operations.
  This corresponds to the behavior of the command line options
  :option:`-moutline-atomics` and :option:`-mno-outline-atomics`.

The above target attributes can be specified as follows:

.. code-block:: c++

  __attribute__((target("attr-string")))
  int
  f (int a)
  {
    return a + 5;
  }

where ``attr-string`` is one of the attribute strings specified above.

Additionally, the architectural extension string may be specified on its
own.  This can be used to turn on and off particular architectural extensions
without having to specify a particular architecture version or core.  Example:

.. code-block:: c++

  __attribute__((target("+crc+nocrypto")))
  int
  foo (int a)
  {
    return a + 5;
  }

In this example ``target("+crc+nocrypto")`` enables the ``crc``
extension and disables the ``crypto`` extension for the function ``foo``
without modifying an existing :option:`-march=` or :option:`-mcpu` option.

Multiple target function attributes can be specified by separating them with
a comma.  For example:

.. code-block:: c++

  __attribute__((target("arch=armv8-a+crc+crypto,tune=cortex-a53")))
  int
  foo (int a)
  {
    return a + 5;
  }

is valid and compiles function ``foo`` for ARMv8-A with ``crc``
and ``crypto`` extensions and tunes it for ``cortex-a53``.

Inlining rules
~~~~~~~~~~~~~~

Specifying target attributes on individual functions or performing link-time
optimization across translation units compiled with different target options
can affect function inlining rules:

In particular, a caller function can inline a callee function only if the
architectural features available to the callee are a subset of the features
available to the caller.
For example: A function ``foo`` compiled with :option:`-march=armv8-a+crc`,
or tagged with the equivalent ``arch=armv8-a+crc`` attribute,
can inline a function ``bar`` compiled with :option:`-march=armv8-a+nocrc`
because the all the architectural features that function ``bar`` requires
are available to function ``foo``.  Conversely, function ``bar`` cannot
inline function ``foo``.

Additionally inlining a function compiled with :option:`-mstrict-align` into a
function compiled without ``-mstrict-align`` is not allowed.
However, inlining a function compiled without :option:`-mstrict-align` into a
function compiled with :option:`-mstrict-align` is allowed.

Note that CPU tuning options and attributes such as the :option:`-mcpu=`,
:option:`-mtune=` do not inhibit inlining unless the CPU specified by the
:option:`-mcpu=` option or the :gcc-attr:`cpu=` attribute conflicts with the
architectural feature rules specified above.
