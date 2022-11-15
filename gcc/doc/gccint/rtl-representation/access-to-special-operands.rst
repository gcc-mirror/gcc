..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: access to special operands

.. _special-accessors:

Access to Special Operands
**************************

Some RTL nodes have special annotations associated with them.

``MEM``

  .. index:: MEM_ALIAS_SET

  :samp:`MEM_ALIAS_SET ({x})`
    If 0, :samp:`{x}` is not in any alias set, and may alias anything.  Otherwise,
    :samp:`{x}` can only alias ``MEM`` s in a conflicting alias set.  This value
    is set in a language-dependent manner in the front-end, and should not be
    altered in the back-end.  In some front-ends, these numbers may correspond
    in some way to types, or other language-level entities, but they need not,
    and the back-end makes no such assumptions.
    These set numbers are tested with ``alias_sets_conflict_p``.

    .. index:: MEM_EXPR

  :samp:`MEM_EXPR ({x})`
    If this register is known to hold the value of some user-level
    declaration, this is that tree node.  It may also be a
    ``COMPONENT_REF``, in which case this is some field reference,
    and ``TREE_OPERAND (x, 0)`` contains the declaration,
    or another ``COMPONENT_REF``, or null if there is no compile-time
    object associated with the reference.

    .. index:: MEM_OFFSET_KNOWN_P

  :samp:`MEM_OFFSET_KNOWN_P ({x})`
    True if the offset of the memory reference from ``MEM_EXPR`` is known.
    :samp:`MEM_OFFSET ({x})` provides the offset if so.

    .. index:: MEM_OFFSET

  :samp:`MEM_OFFSET ({x})`
    The offset from the start of ``MEM_EXPR``.  The value is only valid if
    :samp:`MEM_OFFSET_KNOWN_P ({x})` is true.

    .. index:: MEM_SIZE_KNOWN_P

  :samp:`MEM_SIZE_KNOWN_P ({x})`
    True if the size of the memory reference is known.
    :samp:`MEM_SIZE ({x})` provides its size if so.

    .. index:: MEM_SIZE

  :samp:`MEM_SIZE ({x})`
    The size in bytes of the memory reference.
    This is mostly relevant for ``BLKmode`` references as otherwise
    the size is implied by the mode.  The value is only valid if
    :samp:`MEM_SIZE_KNOWN_P ({x})` is true.

    .. index:: MEM_ALIGN

  :samp:`MEM_ALIGN ({x})`
    The known alignment in bits of the memory reference.

    .. index:: MEM_ADDR_SPACE

  :samp:`MEM_ADDR_SPACE ({x})`
    The address space of the memory reference.  This will commonly be zero
    for the generic address space.

``REG``

  .. index:: ORIGINAL_REGNO

  :samp:`ORIGINAL_REGNO ({x})`
    This field holds the number the register 'originally' had; for a
    pseudo register turned into a hard reg this will hold the old pseudo
    register number.

    .. index:: REG_EXPR

  :samp:`REG_EXPR ({x})`
    If this register is known to hold the value of some user-level
    declaration, this is that tree node.

    .. index:: REG_OFFSET

  :samp:`REG_OFFSET ({x})`
    If this register is known to hold the value of some user-level
    declaration, this is the offset into that logical storage.

.. envvar:: SYMBOL_REF

  .. index:: SYMBOL_REF_DECL

  :samp:`SYMBOL_REF_DECL ({x})`
    If the ``symbol_ref`` :samp:`{x}` was created for a ``VAR_DECL`` or
    a ``FUNCTION_DECL``, that tree is recorded here.  If this value is
    null, then :samp:`{x}` was created by back end code generation routines,
    and there is no associated front end symbol table entry.

    ``SYMBOL_REF_DECL`` may also point to a tree of class ``'c'``,
    that is, some sort of constant.  In this case, the ``symbol_ref``
    is an entry in the per-file constant pool; again, there is no associated
    front end symbol table entry.

    .. index:: SYMBOL_REF_CONSTANT

  :samp:`SYMBOL_REF_CONSTANT ({x})`
    If :samp:`CONSTANT_POOL_ADDRESS_P ({x})` is true, this is the constant
    pool entry for :samp:`{x}`.  It is null otherwise.

    .. index:: SYMBOL_REF_DATA

  :samp:`SYMBOL_REF_DATA ({x})`
    A field of opaque type used to store ``SYMBOL_REF_DECL`` or
    ``SYMBOL_REF_CONSTANT``.

    .. index:: SYMBOL_REF_FLAGS

  :samp:`SYMBOL_REF_FLAGS ({x})`
    In a ``symbol_ref``, this is used to communicate various predicates
    about the symbol.  Some of these are common enough to be computed by
    common code, some are specific to the target.  The common bits are:

    .. index:: SYMBOL_REF_FUNCTION_P, SYMBOL_FLAG_FUNCTION

    .. envvar:: SYMBOL_FLAG_FUNCTION

      Set if the symbol refers to a function.

    .. envvar:: SYMBOL_FLAG_LOCAL

      Set if the symbol is local to this 'module'.
      See ``TARGET_BINDS_LOCAL_P``.

    .. envvar:: SYMBOL_FLAG_EXTERNAL

      Set if this symbol is not defined in this translation unit.
      Note that this is not the inverse of ``SYMBOL_FLAG_LOCAL``.

    .. envvar:: SYMBOL_FLAG_SMALL

      Set if the symbol is located in the small data section.
      See ``TARGET_IN_SMALL_DATA_P``.

    :samp:`SYMBOL_REF_TLS_MODEL ({x})`
      This is a multi-bit field accessor that returns the ``tls_model``
      to be used for a thread-local storage symbol.  It returns zero for
      non-thread-local symbols.

      .. index:: SYMBOL_REF_HAS_BLOCK_INFO_P, SYMBOL_FLAG_HAS_BLOCK_INFO

    .. envvar:: SYMBOL_FLAG_HAS_BLOCK_INFO

      Set if the symbol has ``SYMBOL_REF_BLOCK`` and
      ``SYMBOL_REF_BLOCK_OFFSET`` fields.

      .. index:: -fsection-anchors

    .. envvar:: SYMBOL_FLAG_ANCHOR

      Set if the symbol is used as a section anchor.  'Section anchors'
      are symbols that have a known position within an ``object_block``
      and that can be used to access nearby members of that block.
      They are used to implement :option:`-fsection-anchors`.

      If this flag is set, then ``SYMBOL_FLAG_HAS_BLOCK_INFO`` will be too.

    Bits beginning with ``SYMBOL_FLAG_MACH_DEP`` are available for
    the target's use.

:samp:`SYMBOL_REF_BLOCK ({x})`
  If :samp:`SYMBOL_REF_HAS_BLOCK_INFO_P ({x})`, this is the
  :samp:`object_block` structure to which the symbol belongs,
  or ``NULL`` if it has not been assigned a block.

  .. index:: SYMBOL_REF_BLOCK_OFFSET

:samp:`SYMBOL_REF_BLOCK_OFFSET ({x})`
  If :samp:`SYMBOL_REF_HAS_BLOCK_INFO_P ({x})`, this is the offset of :samp:`{x}`
  from the first object in :samp:`SYMBOL_REF_BLOCK ({x})`.  The value is
  negative if :samp:`{x}` has not yet been assigned to a block, or it has not
  been given an offset within that block.
