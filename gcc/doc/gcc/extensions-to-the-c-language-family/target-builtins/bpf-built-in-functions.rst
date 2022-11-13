..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _bpf-built-in-functions:

BPF Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^

The following built-in functions are available for eBPF targets.

.. function:: unsigned long long __builtin_bpf_load_byte (unsigned long long offset)

  Load a byte from the ``struct sk_buff`` packet data pointed by the register ``%r6`` and return it.

.. function:: unsigned long long __builtin_bpf_load_half (unsigned long long offset)

  Load 16-bits from the ``struct sk_buff`` packet data pointed by the register ``%r6`` and return it.

.. function:: unsigned long long __builtin_bpf_load_word (unsigned long long offset)

  Load 32-bits from the ``struct sk_buff`` packet data pointed by the register ``%r6`` and return it.

.. function:: void * __builtin_preserve_access_index (expr)

  BPF Compile Once-Run Everywhere (CO-RE) support. Instruct GCC to generate CO-RE relocation records for any accesses to aggregate data structures (struct, union, array types) in :samp:`{expr}`. This builtin is otherwise transparent, the return value is whatever :samp:`{expr}` evaluates to. It is also overloaded: :samp:`{expr}` may be of any type (not necessarily a pointer), the return type is the same. Has no effect if ``-mco-re`` is not in effect (either specified or implied).

.. function:: unsigned int __builtin_preserve_field_info (expr, unsigned int kind)

  BPF Compile Once-Run Everywhere (CO-RE) support. This builtin is used to
  extract information to aid in struct/union relocations.  :samp:`{expr}` is
  an access to a field of a struct or union. Depending on :samp:`{kind}`, different
  information is returned to the program. A CO-RE relocation for the access in
  :samp:`{expr}` with kind :samp:`{kind}` is recorded if ``-mco-re`` is in effect.

  The following values are supported for :samp:`{kind}` :

  :samp:`{FIELD_BYTE_OFFSET = 0}`
    The returned value is the offset, in bytes, of the field from the
    beginning of the containing structure. For bitfields, the byte offset
    of the containing word.

  :samp:`{FIELD_BYTE_SIZE = 1}`
    The returned value is the size, in bytes, of the field. For bitfields,
    the size in bytes of the containing word.

  :samp:`{FIELD_EXISTENCE = 2}`
    The returned value is 1 if the field exists, 0 otherwise. Always 1 at
    compile time.

  :samp:`{FIELD_SIGNEDNESS = 3}`
    The returned value is 1 if the field is signed, 0 otherwise.

  :samp:`{FIELD_LSHIFT_U64 = 4}` :samp:`{FIELD_RSHIFT_U64 = 5}`
    The returned value is the number of bits of left- or right-shifting
    respectively needed in order to recover the original value of the field,
    after it has been loaded by a read of FIELD_BYTE_SIZE bytes into an
    unsigned 64-bit value. Primarily useful for reading bitfield values
    from structures which may change between kernel versions.

  Note that the return value is a constant which is known at
  compile-time. If the field has a variable offset then
  FIELD_BYTE_OFFSET, FIELD_LSHIFT_U64 and FIELD_RSHIFT_U64 are not
  supported. Similarly, if the field has a variable size then
  FIELD_BYTE_SIZE, FIELD_LSHIFT_U64 and FIELD_RSHIFT_U64 are not
  supported.

  For example, __builtin_preserve_field_info can be used to reliably
  extract bitfield values from a structure which may change between
  kernel versions:

  .. code-block:: c++

    struct S
    {
      short a;
      int x:7;
      int y:5;
    };

    int
    read_y (struct S *arg)
    {
      unsigned long long val;
      unsigned int offset = __builtin_preserve_field_info (arg->y, FIELD_BYTE_OFFSET);
      unsigned int size = __builtin_presrve_field_info (arg->y, FIELD_BYTE_SIZE);

      /* Read size bytes from arg + offset into val.  */
      bpf_probe_read (&val, size, arg + offset);

      val <<= __builtin_preserve_field_info (arg->y, FIELD_LSHIFT_U64);

      if (__builtin_preserve_field_info (arg->y, FIELD_SIGNEDNESS))
        val = ((long long) val >> __builtin_preserve_field_info (arg->y, FIELD_RSHIFT_U64));
      else
        val >>= __builtin_preserve_field_info (arg->y, FIELD_RSHIFT_U64);

      return val;
    }