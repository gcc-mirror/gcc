..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _nios-ii-function-attributes:

Nios II Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the Nios II back end:

.. index:: target function attribute

.. nios-ii-fn-attr:: target (options)

  As discussed in :ref:`common-function-attributes`, this attribute
  allows specification of target-specific compilation options.

  When compiling for Nios II, the following options are allowed:

  :samp:`custom-{insn}={N}` :samp:`no-custom-{insn}`

    .. index:: target("custom-insn=N") function attribute, Nios II, target("no-custom-insn") function attribute, Nios II

    Each :samp:`custom-{insn}={N}` attribute locally enables use of a
    custom instruction with encoding :samp:`{N}` when generating code that uses
    :samp:`{insn}`.  Similarly, :samp:`no-custom-{insn}` locally inhibits use of
    the custom instruction :samp:`{insn}`.
    These target attributes correspond to the
    :option:`-mcustom-insn=N` and :option:`-mno-custom-insn`
    command-line options, and support the same set of :samp:`{insn}` keywords.
    See :ref:`nios-ii-options`, for more information.

  :samp:`custom-fpu-cfg={name}`

    .. index:: target("custom-fpu-cfg=name") function attribute, Nios II

    This attribute corresponds to the :option:`-mcustom-fpu-cfg=name`
    command-line option, to select a predefined set of custom instructions
    named :samp:`{name}`.
    See :ref:`nios-ii-options`, for more information.
