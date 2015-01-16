TODOs
-----

API
===
* error-handling:
    * have a client-provided error-handling callback for the context, and
      call it, rather than asserting/crashing etc, to make the API resilient and helpful

* probably should turn off signal handlers and backtracing, leaving that to
  the client code

* enums and ABI: give enums specific numbers, in ranges, to make it
  possible to maintain a logical ordering whilst preserving ABI.

* expose the statements in the API? (mostly so they can be stringified?)

* support more arithmetic ops and comparison modes

* access to a function by address::

    extern gcc_jit_function *
    gcc_jit_context_get_function (ctxt,
                                  void *); /* need type information */

  so you can access "static" fns in your code.

* ability to turn a function into a function pointer::

    gcc_jit_function_as_rvalue ()

* expressing branch probabilies (like __builtin_expect)::

    extern gcc_jit_rvalue *
    gcc_jit_rvalue_likely (gcc_jit_rvalue *rvalue,
                           int is_likely);

  though would:

    extern void
    gcc_jit_block_set_likelihood (gcc_jit_block *block,
                                  int hotness);

  be better?  (for expressing how hot the current location is)

* add a SONAME to the library (and potentially version the symbols?)

* do we need alternative forms of division (floor vs rounding)?

* are we missing any ops?

* error-checking:

    * gcc_jit_context_new_unary_op: various checks needed

    * gcc_jit_context_new_binary_op: various checks needed

    * gcc_jit_context_new_comparison: must be numeric or pointer types

    * gcc_jit_context_new_array_access: "index" must be of numeric type.

    * gcc_jit_lvalue_access_field: must be field of correct struct

    * gcc_jit_rvalue_access_field: must be field of correct struct

    * gcc_jit_block_add_assignment_op: check the types

* Implement more kinds of casts e.g. pointers

Bugs
====
* fixing all the state issues: make it work repeatedly with optimization
  turned up to full.

* make the dirty dirty hacks less egregious...

* test under valgrind; fix memory leaks

* re-architect gcc so we don't have to reinitialize everything every time
  a context is compiled

Test suite
==========
* measure code coverage in testing of libgccjit.so

Future milestones
=================
* try porting llvmpipe to gcc

* inline assembler?

* Detect and issue warnings/errors about uses of uninitialized variables

* Warn about unused objects in a context (e.g. rvalues/lvalues)?  (e.g.
  for gcc_jit_context_new_call vs gcc_jit_block_add_eval)

Nice to have
============
* Currently each function has a single stmt_list, which is built in
  postprocessing by walking the list of blocks.  Presumably we could
  have each block have its own stmt_list, avoiding the need for this
  traversal, and having the block structure show up within tree dumps.
  Alternatively, could we skip tree and go straight to gimple?

* ability to give contexts names, for ease of debugging?


Probably not needed
===================
* "switch" and "case" ?

* sizeof (should this be an API hook?)  do we even need it? presumably
  client code can just do the sizeof() in its own code.

* do we need unary plus?

etc etc
