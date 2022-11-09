..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: SSA, RTL form, RTL SSA

.. _rtl-ssa:

On-the-Side SSA Form for RTL
****************************

The patterns of an individual RTL instruction describe which registers
are inputs to that instruction and which registers are outputs from
that instruction.  However, it is often useful to know where the
definition of a register input comes from and where the result of
a register output is used.  One way of obtaining this information
is to use the RTL SSA form, which provides a Static Single Assignment
representation of the RTL instructions.

The RTL SSA code is located in the :samp:`rtl-ssa` subdirectory of the GCC
source tree.  This section only gives a brief overview of it; please
see the comments in the source code for more details.

.. toctree::
  :maxdepth: 2


.. _using-rtl-ssa:

Using RTL SSA in a pass
^^^^^^^^^^^^^^^^^^^^^^^

A pass that wants to use the RTL SSA form should start with the following:

.. code-block:: c++

  #define INCLUDE_ALGORITHM
  #define INCLUDE_FUNCTIONAL
  #include "config.h"
  #include "system.h"
  #include "coretypes.h"
  #include "backend.h"
  #include "rtl.h"
  #include "df.h"
  #include "rtl-ssa.h"

All the RTL SSA code is contained in the ``rtl_ssa`` namespace,
so most passes will then want to do:

.. code-block:: c++

  using namespace rtl_ssa;

However, this is purely a matter of taste, and the examples in the rest of
this section do not require it.

The RTL SSA represention is an optional on-the-side feature that applies
on top of the normal RTL instructions.  It is currently local to individual
RTL passes and is not maintained across passes.

However, in order to allow the RTL SSA information to be preserved across
passes in future, :samp:`crtl->ssa` points to the current function's
SSA form (if any).  Passes that want to use the RTL SSA form should
first do:

.. code-block:: c++

  crtl->ssa = new rtl_ssa::function_info (fn);

where :samp:`{fn}` is the function that the pass is processing.
(Passes that are ``using namespace rtl_ssa`` do not need
the :samp:`rtl_ssa::`.)

Once the pass has finished with the SSA form, it should do the following:

.. code-block:: c++

  free_dominance_info (CDI_DOMINATORS);
  if (crtl->ssa->perform_pending_updates ())
    cleanup_cfg (0);

  delete crtl->ssa;
  crtl->ssa = nullptr;

The ``free_dominance_info`` call is necessary because
dominance information is not currently maintained between RTL passes.
The next two lines commit any changes to the RTL instructions that
were queued for later; see the comment above the declaration of
``perform_pending_updates`` for details.  The final two lines
discard the RTL SSA form and free the associated memory.

.. index:: RPO, reverse postorder, instructions, RTL SSA, rtl_ssa::insn_info

.. _rtl-ssa-instructions:

RTL SSA Instructions
^^^^^^^^^^^^^^^^^^^^

RTL SSA instructions are represented by an ``rtl_ssa::insn_info``.
These instructions are chained together in a single list that follows
a reverse postorder (RPO) traversal of the function.  This means that
if any path through the function can execute an instruction :samp:`{I1}`
and then later execute an instruction :samp:`{I2}` for the first time,
:samp:`{I1}` appears before :samp:`{I2}` in the list.
Note that this
order is different from the order of the underlying RTL instructions,
which follow machine code order instead.

Two RTL SSA instructions can be compared to find which instruction
occurs earlier than the other in the RPO.  One way to do this is
to use the C++ comparison operators, such as:

.. code-block:: c++

  *insn1 < *insn2

Another way is to use the ``compare_with`` function:

.. code-block:: c++

  insn1->compare_with (insn2)

This expression is greater than zero if :samp:`{insn1}` comes after :samp:`{insn2}`
in the RPO, less than zero if :samp:`{insn1}` comes before :samp:`{insn2}` in the
RPO, or zero if :samp:`{insn1}` and :samp:`{insn2}` are the same.  This order is
maintained even if instructions are added to the function or moved around.

The main purpose of ``rtl_ssa::insn_info`` is to hold
SSA information about an instruction.  However, it also caches
certain properties of the instruction, such as whether it is an
inline assembly instruction, whether it has volatile accesses, and so on.

.. index:: basic blocks, RTL SSA, basic_block, rtl_ssa::bb_info

.. _rtl-ssa-basic-blocks:

RTL SSA Basic Blocks
^^^^^^^^^^^^^^^^^^^^

RTL SSA instructions (see :ref:`rtl-ssa-instructions`) are organized into
basic blocks, with each block being represented by an ``rtl_ssa:bb_info``.
There is a one-to-one mapping between these ``rtl_ssa:bb_info``
structures and the underlying CFG ``basic_block`` structures
(see :ref:`basic-blocks`).

.. index:: 'real' instructions, RTL SSA

.. _real-rtl-ssa-insns:

If a CFG basic block :samp:`{bb}` contains an RTL instruction :samp:`{insn}`,
the RTL SSA represenation of :samp:`{bb}` also contains an RTL SSA representation
of :samp:`{insn}`.
Note that this excludes non-instruction things like
``note`` s and ``barrier`` s that also appear in the chain of RTL
instructions.

Within RTL SSA, these instructions are referred to as
'real' instructions.  These real instructions fall into two groups:
debug instructions and nondebug instructions.  Only nondebug instructions
should affect code generation decisions.

In addition, each RTL SSA basic block has two 'artificial'
instructions: a 'head' instruction that comes before all the real
instructions and an 'end' instruction that comes after all real
instructions.  These instructions exist to represent things that
are conceptually defined or used at the start and end of a basic block.
The instructions always exist, even if they do not currently do anything.

Like instructions, these blocks are chained together in a reverse
postorder.  This list includes the entry block (which always comes
first) and the exit block (which always comes last).

.. index:: extended basic blocks, RTL SSA, rtl_ssa::ebb_info

RTL SSA basic blocks are chained together into 'extended basic blocks'
(EBBs), represented by an ``rtl_ssa::ebb_info``.  Extended basic
blocks contain one or more basic blocks.  They have the property
that if a block :samp:`{bby}` comes immediately after a block :samp:`{bbx}`
in an EBB, then :samp:`{bby}` can only be reached by :samp:`{bbx}` ; in other words,
:samp:`{bbx}` is the sole predecessor of :samp:`{bby}`.

Each extended basic block starts with an artificial 'phi node'
instruction.  This instruction defines all phi nodes for the EBB
(see :ref:`rtl-ssa-phi-nodes`).  (Individual blocks in an EBB do not
need phi nodes because their live values can only come from one source.)

The contents of a function are therefore represented using a
four-level hierarchy:

* functions (``rtl_ssa::function_info``), which contain ...

* extended basic blocks (``rtl_ssa::ebb_info``), which contain ...

* basic blocks (``rtl_ssa::bb_info``), which contain ...

* instructions (``rtl_ssa::insn_info``)

In dumps, a basic block is identified as ``bbn``, where :samp:`{n}`
is the index of the associated CFG ``basic_block`` structure.
An EBB is in turn identified by the index of its first block.
For example, an EBB that contains :samp:`bb10`, ``bb5``, ``bb6``
and ``bb9`` is identified as :samp:`{ebb10}`.

.. _rtl-ssa-resources:

RTL SSA Resources
^^^^^^^^^^^^^^^^^

The RTL SSA form tracks two types of 'resource': registers and memory.
Each hard and pseudo register is a separate resource.  Memory is a
single unified resource, like it is in GIMPLE (see :ref:`gimple`).

Each resource has a unique identifier.  The unique identifier for a
register is simply its register number.  The unique identifier for
memory is a special register number called ``MEM_REGNO``.

Since resource numbers so closely match register numbers, it is sometimes
convenient to refer to them simply as register numbers, or 'regnos'
for short.  However, the RTL SSA form also provides an abstraction
of resources in the form of ``rtl_ssa::resource_info``.
This is a lightweight class that records both the regno of a resource
and the ``machine_mode`` that the resource has (see :ref:`machine-modes`).
It has functions for testing whether a resource is a register or memory.
In principle it could be extended to other kinds of resource in future.

.. _rtl-ssa-accesses:

RTL SSA Register and Memory Accesses
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In the RTL SSA form, most reads or writes of a resource are
represented as a ``rtl_ssa::access_info``.
The exceptions
are call clobbers, which are generally represented separately.
See the comment above ``rtl_ssa::insn_info`` for details.

These ``rtl_ssa::access_info`` s are organized into the following
class hierarchy:

.. index:: rtl_ssa::access_info, rtl_ssa::use_info, rtl_ssa::def_info, rtl_ssa::clobber_info, rtl_ssa::set_info, rtl_ssa::phi_info

.. code-block:: c++

  rtl_ssa::access_info
    |
    +-- rtl_ssa::use_info
    |
    +-- rtl_ssa::def_info
          |
          +-- rtl_ssa::clobber_info
          |
          +-- rtl_ssa::set_info
                |
                +-- rtl_ssa::phi_info

A ``rtl_ssa::use_info`` represents a read or use of a resource and
a ``rtl_ssa::def_info`` represents a write or definition of a resource.
As in the main RTL representation, there are two basic types of
definition: clobbers and sets.  The difference is that a clobber
leaves the register with an unspecified value that cannot be used
or relied on by later instructions, while a set leaves the register
with a known value that later instructions could use if they wanted to.
A ``rtl_ssa::clobber_info`` represents a clobber and
a ``rtl_ssa::set_info`` represent a set.

Each ``rtl_ssa::use_info`` records which single ``rtl_ssa::set_info``
provides the value of the resource; this is null if the resource is
completely undefined at the point of use.  Each ``rtl_ssa::set_info``
in turn records all the ``rtl_ssa::use_info`` s that use its value.

If a value of a resource can come from multiple sources,
a ``rtl_ssa::phi_info`` brings those multiple sources together
into a single definition (see :ref:`rtl-ssa-phi-nodes`).

.. index:: phi nodes, RTL SSA, rtl_ssa::phi_info

.. _rtl-ssa-phi-nodes:

RTL SSA Phi Nodes
^^^^^^^^^^^^^^^^^

If a resource is live on entry to an extended basic block and if the
resource's value can come from multiple sources, the extended basic block
has a 'phi node' that collects together these multiple sources.
The phi node conceptually has one input for each incoming edge of
the extended basic block, with the input specifying the value of
the resource on that edge.  For example, suppose a function contains
the following RTL:

.. code-block:: c++

  ;; Basic block bb3
  ...
  (set (reg:SI R1) (const_int 0))  ;; A
  (set (pc) (label_ref bb5))

  ;; Basic block bb4
  ...
  (set (reg:SI R1) (const_int 1))  ;; B
  ;; Fall through

  ;; Basic block bb5
  ;; preds: bb3, bb4
  ;; live in: R1 ...
  (code_label bb5)
  ...
  (set (reg:SI R2)
       (plus:SI (reg:SI R1) ...))  ;; C

The value of R1 on entry to block 5 can come from either A or B.
The extended basic block that contains block 5 would therefore have a
phi node with two inputs: the first input would have the value of
R1 defined by A and the second input would have the value of
R1 defined by B.  This phi node would then provide the value of
R1 for C (assuming that R1 does not change again between
the start of block 5 and C).

Since RTL is not a 'native' SSA representation, these phi nodes
simply collect together definitions that already exist.  Each input
to a phi node for a resource :samp:`{R}` is itself a definition of
resource :samp:`{R}` (or is null if the resource is completely
undefined for a particular incoming edge).  This is in contrast
to a native SSA representation like GIMPLE, where the phi inputs
can be arbitrary expressions.  As a result, RTL SSA phi nodes
never involve 'hidden' moves: all moves are instead explicit.

Phi nodes are represented as a ``rtl_ssa::phi_node``.
Each input to a phi node is represented as an ``rtl_ssa::use_info``.

.. _rtl-ssa-access-lists:

RTL SSA Access Lists
^^^^^^^^^^^^^^^^^^^^

All the definitions of a resource are chained together in reverse postorder.
In general, this list can contain an arbitrary mix of both sets
(``rtl_ssa::set_info``) and clobbers (``rtl_ssa::clobber_info``).
However, it is often useful to skip over all intervening clobbers
of a resource in order to find the next set.  The list is constructed
in such a way that this can be done in amortized constant time.

All uses (``rtl_ssa::use_info``) of a given set are also chained
together into a list.  This list of uses is divided into three parts:

* uses by 'real' nondebug instructions (see :ref:`real-rtl-ssa-insns`)

* uses by real debug instructions

* uses by phi nodes (see :ref:`rtl-ssa-phi-nodes`)

The first and second parts individually follow reverse postorder.
The third part has no particular order.

.. index:: degenerate phi node, RTL SSA

The last use by a real nondebug instruction always comes earlier in
the reverse postorder than the next definition of the resource (if any).
This means that the accesses follow a linear sequence of the form:

* first definition of resource R

  * first use by a real nondebug instruction of the first definition of resource R

  * ...

  * last use by a real nondebug instruction of the first definition of resource R

* second definition of resource R

  * first use by a real nondebug instruction of the second definition of resource R

  * ...

  * last use by a real nondebug instruction of the second definition of resource R

* ...

* last definition of resource R

  * first use by a real nondebug instruction of the last definition of resource R

  * ...

  * last use by a real nondebug instruction of the last definition of resource R

(Note that clobbers never have uses; only sets do.)

This linear view is easy to achieve when there is only a single definition
of a resource, which is commonly true for pseudo registers.  However,
things are more complex  if code has a structure like the following:

.. code-block:: c++

  // ebb2, bb2
  R = va;        // A
  if (...)
    {
      // ebb2, bb3
      use1 (R);  // B
      ...
      R = vc;    // C
    }
  else
    {
      // ebb4, bb4
      use2 (R);  // D
    }

The list of accesses would begin as follows:

* definition of R by A

  * use of A's definition of R by B

* definition of R by C

The next access to R is in D, but the value of R that D uses comes from
A rather than C.

This is resolved by adding a phi node for ``ebb4``.  All inputs to this
phi node have the same value, which in the example above is A's definition
of R.  In other circumstances, it would not be necessary to create a phi
node when all inputs are equal, so these phi nodes are referred to as
'degenerate' phi nodes.

The full list of accesses to R is therefore:

* definition of R by A

  * use of A's definition of R by B

* definition of R by C

* definition of R by ebb4's phi instruction, with the input coming from A

  * use of the ebb4's R phi definition of R by B

Note that A's definition is also used by ebb4's phi node, but this
use belongs to the third part of the use list described above and
so does not form part of the linear sequence.

It is possible to 'look through' any degenerate phi to the ultimate
definition using the function ``look_through_degenerate_phi``.
Note that the input to a degenerate phi is never itself provided
by a degenerate phi.

At present, the SSA form takes this principle one step further
and guarantees that, for any given resource :samp:`{res}`, one of the
following is true:

* The resource has a single definition :samp:`{def}`, which is not a phi node.
  Excluding uses of undefined registers, all uses of :samp:`{res}` by real
  nondebug instructions use the value provided by :samp:`{def}`.

* Excluding uses of undefined registers, all uses of :samp:`{res}` use
  values provided by definitions that occur earlier in the same
  extended basic block.  These definitions might come from phi nodes
  or from real instructions.

.. index:: rtl_ssa::insn_change

.. _changing-rtl-instructions:

Using the RTL SSA framework to change instructions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

There are various routines that help to change a single RTL instruction
or a group of RTL instructions while keeping the RTL SSA form up-to-date.
This section first describes the process for changing a single instruction,
then goes on to describe the differences when changing multiple instructions.

.. toctree::
  :maxdepth: 2


.. _changing-one-rtl-ssa-instruction:

Changing One RTL SSA Instruction
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Before making a change, passes should first use a statement like the
following:

.. code-block:: c++

  auto attempt = crtl->ssa->new_change_attempt ();

Here, ``attempt`` is an RAII object that should remain in scope
for the entire change attempt.  It automatically frees temporary
memory related to the changes when it goes out of scope.

Next, the pass should create an ``rtl_ssa::insn_change`` object
for the instruction that it wants to change.  This object specifies
several things:

* what the instruction's new list of uses should be (``new_uses``).
  By default this is the same as the instruction's current list of uses.

* what the instruction's new list of definitions should be (``new_defs``).
  By default this is the same as the instruction's current list of
  definitions.

* where the instruction should be located (``move_range``).
  This is a range of instructions after which the instruction could
  be placed, represented as an ``rtl_ssa::insn_range``.
  By default the instruction must remain at its current position.

If a pass was attempting to change all these properties of an instruction
``insn``, it might do something like this:

.. code-block:: c++

  rtl_ssa::insn_change change (insn);
  change.new_defs = ...;
  change.new_uses = ...;
  change.move_range = ...;

This ``rtl_ssa::insn_change`` only describes something that the
pass *might* do; at this stage, nothing has actually changed.

As noted above, the default ``move_range`` requires the instruction
to remain where it is.  At the other extreme, it is possible to allow
the instruction to move anywhere within its extended basic block,
provided that all the new uses and definitions can be performed
at the new location.  The way to do this is:

.. code-block:: c++

  change.move_range = insn->ebb ()->insn_range ();

In either case, the next step is to make sure that move range is
consistent with the new uses and definitions.  The way to do this is:

.. code-block:: c++

  if (!rtl_ssa::restrict_movement (change))
    return false;

This function tries to limit ``move_range`` to a range of instructions
at which ``new_uses`` and ``new_defs`` can be correctly performed.
It returns true on success or false if no suitable location exists.

The pass should also tentatively change the pattern of the instruction
to whatever form the pass wants the instruction to have.  This should use
the facilities provided by :samp:`recog.cc`.  For example:

.. code-block:: c++

  rtl_insn *rtl = insn->rtl ();
  insn_change_watermark watermark;
  validate_change (rtl, &PATTERN (rtl), new_pat, 1);

will tentatively replace ``insn`` 's pattern with ``new_pat``.

These changes and the construction of the ``rtl_ssa::insn_change``
can happen in either order or be interleaved.

After the tentative changes to the instruction are complete,
the pass should check whether the new pattern matches a target
instruction or satisfies the requirements of an inline asm:

.. code-block:: c++

  if (!rtl_ssa::recog (change))
    return false;

This step might change the instruction pattern further in order to
make it match.  It might also add new definitions or restrict the range
of the move.  For example, if the new pattern did not match in its original
form, but could be made to match by adding a clobber of the flags
register, ``rtl_ssa::recog`` will check whether the flags register
is free at an appropriate point.  If so, it will add a clobber of the
flags register to ``new_defs`` and restrict ``move_range`` to
the locations at which the flags register can be safely clobbered.

Even if the proposed new instruction is valid according to
``rtl_ssa::recog``, the change might not be worthwhile.
For example, when optimizing for speed, the new instruction might
turn out to be slower than the original one.  When optimizing for
size, the new instruction might turn out to be bigger than the
original one.

Passes should check for this case using ``change_is_worthwhile``.
For example:

.. code-block:: c++

  if (!rtl_ssa::change_is_worthwhile (change))
    return false;

If the change passes this test too then the pass can perform the change using:

.. code-block:: c++

  confirm_change_group ();
  crtl->ssa->change_insn (change);

Putting all this together, the change has the following form:

.. code-block:: c++

  auto attempt = crtl->ssa->new_change_attempt ();

  rtl_ssa::insn_change change (insn);
  change.new_defs = ...;
  change.new_uses = ...;
  change.move_range = ...;

  if (!rtl_ssa::restrict_movement (change))
    return false;

  insn_change_watermark watermark;
  // Use validate_change etc. to change INSN's pattern.
  ...
  if (!rtl_ssa::recog (change)
      || !rtl_ssa::change_is_worthwhile (change))
    return false;

  confirm_change_group ();
  crtl->ssa->change_insn (change);

.. _changing-multiple-rtl-ssa-instructions:

Changing Multiple RTL SSA Instructions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The process for changing multiple instructions is similar
to the process for changing single instructions
(see :ref:`changing-one-rtl-ssa-instruction`).  The pass should
again start the change attempt with:

.. code-block:: c++

  auto attempt = crtl->ssa->new_change_attempt ();

and keep ``attempt`` in scope for the duration of the change
attempt.  It should then construct an ``rtl_ssa::insn_change``
for each change that it wants to make.

After this, it should combine the changes into a sequence of
``rtl_ssa::insn_change`` pointers.  This sequence must be in
reverse postorder; the instructions will remain strictly in the
order that the sequence specifies.

For example, if a pass is changing exactly two instructions,
it might do:

.. code-block:: c++

  rtl_ssa::insn_change *changes[] = { &change1, change2 };

where ``change1`` 's instruction must come before ``change2`` 's.
Alternatively, if the pass is changing a variable number of
instructions, it might build up the sequence in a
``vec<rtl_ssa::insn_change *>``.

By default, ``rtl_ssa::restrict_movement`` assumes that all
instructions other than the one passed to it will remain in their
current positions and will retain their current uses and definitions.
When changing multiple instructions, it is usually more effective
to ignore the other instructions that are changing.  The sequencing
described above ensures that the changing instructions remain
in the correct order with respect to each other.
The way to do this is:

.. code-block:: c++

  if (!rtl_ssa::restrict_movement (change, insn_is_changing (changes)))
    return false;

Similarly, when ``rtl_ssa::restrict_movement`` is detecting
whether a register can be clobbered, it by default assumes that
all other instructions will remain in their current positions and
retain their current form.  It is again more effective to ignore
changing instructions (which might, for example, no longer need
to clobber the flags register).  The way to do this is:

.. code-block:: c++

  if (!rtl_ssa::recog (change, insn_is_changing (changes)))
    return false;

When changing multiple instructions, the important question is usually
not whether each individual change is worthwhile, but whether the changes
as a whole are worthwhile.  The way to test this is:

.. code-block:: c++

  if (!rtl_ssa::changes_are_worthwhile (changes))
    return false;

The process for changing single instructions makes sure that one
``rtl_ssa::insn_change`` in isolation is valid.  But when changing
multiple instructions, it is also necessary to test whether the
sequence as a whole is valid.  For example, it might be impossible
to satisfy all of the ``move_range`` s at once.

Therefore, once the pass has a sequence of changes that are
individually correct, it should use:

.. code-block:: c++

  if (!crtl->ssa->verify_insn_changes (changes))
    return false;

to check whether the sequence as a whole is valid.  If all checks pass,
the final step is:

.. code-block:: c++

  confirm_change_group ();
  crtl->ssa->change_insns (changes);

Putting all this together, the process for a two-instruction change is:

.. code-block:: c++

  auto attempt = crtl->ssa->new_change_attempt ();

  rtl_ssa::insn_change change (insn1);
  change1.new_defs = ...;
  change1.new_uses = ...;
  change1.move_range = ...;

  rtl_ssa::insn_change change (insn2);
  change2.new_defs = ...;
  change2.new_uses = ...;
  change2.move_range = ...;

  rtl_ssa::insn_change *changes[] = { &change1, change2 };

  auto is_changing = insn_is_changing (changes);
  if (!rtl_ssa::restrict_movement (change1, is_changing)
      || !rtl_ssa::restrict_movement (change2, is_changing))
    return false;

  insn_change_watermark watermark;
  // Use validate_change etc. to change INSN1's and INSN2's patterns.
  ...
  if (!rtl_ssa::recog (change1, is_changing)
      || !rtl_ssa::recog (change2, is_changing)
      || !rtl_ssa::changes_are_worthwhile (changes)
      || !crtl->ssa->verify_insn_changes (changes))
    return false;

  confirm_change_group ();
  crtl->ssa->change_insns (changes);
