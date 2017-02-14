.. Copyright (C) 2015-2017 Free Software Foundation, Inc.
   Originally contributed by David Malcolm <dmalcolm@redhat.com>

   This is free software: you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see
   <http://www.gnu.org/licenses/>.

.. default-domain:: c

Performance
===========

The timing API
--------------

As of GCC 6, libgccjit exposes a timing API, for printing reports on
how long was spent in different parts of code.

You can create a :c:type:`gcc_jit_timer` instance, which will
measure time spent since its creation.  The timer maintains a stack
of "timer items": as control flow moves through your code, you can push
and pop named items relating to your code onto the stack, and the timer
will account the time spent accordingly.

You can also asssociate a timer with a :c:type:`gcc_jit_context`, in
which case the time spent inside compilation will be subdivided.

For example, the following code uses a timer, recording client items
"create_code", "compile", and "running code":

.. code-block:: c

  /* Create a timer.  */
  gcc_jit_timer *timer = gcc_jit_timer_new ();
  if (!timer)
    {
       error ("gcc_jit_timer_new failed");
       return -1;
    }

  /* Let's repeatedly compile and run some code, accumulating it
     all into the timer.  */
  for (int i = 0; i < num_iterations; i++)
    {
      /* Create a context and associate it with the timer.  */
      gcc_jit_context *ctxt = gcc_jit_context_acquire ();
      if (!ctxt)
        {
          error ("gcc_jit_context_acquire failed");
          return -1;
        }
      gcc_jit_context_set_timer (ctxt, timer);

      /* Populate the context, timing it as client item "create_code".  */
      gcc_jit_timer_push (timer, "create_code");
      create_code (ctxt);
      gcc_jit_timer_pop (timer, "create_code");

      /* Compile the context, timing it as client item "compile".  */
      gcc_jit_timer_push (timer, "compile");
      result = gcc_jit_context_compile (ctxt);
      gcc_jit_timer_pop (timer, "compile");

      /* Run the generated code, timing it as client item "running code".  */
      gcc_jit_timer_push (timer, "running code");
      run_the_code (ctxt, result);
      gcc_jit_timer_pop (timer, "running code");

      /* Clean up.  */
      gcc_jit_context_release (ctxt);
      gcc_jit_result_release (result);
  }

  /* Print the accumulated timings.  */
  gcc_jit_timer_print (timer, stderr);
  gcc_jit_timer_release (timer);

giving output like this, showing the internal GCC items at the top, then
client items, then the total::

  Execution times (seconds)
  GCC items:
   phase setup             :   0.29 (14%) usr   0.00 ( 0%) sys   0.32 ( 5%) wall   10661 kB (50%) ggc
   phase parsing           :   0.02 ( 1%) usr   0.00 ( 0%) sys   0.00 ( 0%) wall     653 kB ( 3%) ggc
   phase finalize          :   0.01 ( 1%) usr   0.00 ( 0%) sys   0.00 ( 0%) wall       0 kB ( 0%) ggc
   dump files              :   0.02 ( 1%) usr   0.00 ( 0%) sys   0.01 ( 0%) wall       0 kB ( 0%) ggc
   callgraph construction  :   0.02 ( 1%) usr   0.01 ( 6%) sys   0.01 ( 0%) wall     242 kB ( 1%) ggc
   callgraph optimization  :   0.03 ( 2%) usr   0.00 ( 0%) sys   0.02 ( 0%) wall     142 kB ( 1%) ggc
   trivially dead code     :   0.01 ( 1%) usr   0.00 ( 0%) sys   0.00 ( 0%) wall       0 kB ( 0%) ggc
   df scan insns           :   0.01 ( 1%) usr   0.00 ( 0%) sys   0.00 ( 0%) wall       9 kB ( 0%) ggc
   df live regs            :   0.01 ( 1%) usr   0.00 ( 0%) sys   0.01 ( 0%) wall       0 kB ( 0%) ggc
   inline parameters       :   0.02 ( 1%) usr   0.00 ( 0%) sys   0.01 ( 0%) wall      82 kB ( 0%) ggc
   tree CFG cleanup        :   0.01 ( 1%) usr   0.00 ( 0%) sys   0.00 ( 0%) wall       0 kB ( 0%) ggc
   tree PHI insertion      :   0.01 ( 1%) usr   0.00 ( 0%) sys   0.02 ( 0%) wall      64 kB ( 0%) ggc
   tree SSA other          :   0.01 ( 1%) usr   0.00 ( 0%) sys   0.01 ( 0%) wall      18 kB ( 0%) ggc
   expand                  :   0.01 ( 1%) usr   0.00 ( 0%) sys   0.00 ( 0%) wall     398 kB ( 2%) ggc
   jump                    :   0.01 ( 1%) usr   0.00 ( 0%) sys   0.00 ( 0%) wall       0 kB ( 0%) ggc
   loop init               :   0.01 ( 0%) usr   0.00 ( 0%) sys   0.00 ( 0%) wall      67 kB ( 0%) ggc
   integrated RA           :   0.02 ( 1%) usr   0.00 ( 0%) sys   0.00 ( 0%) wall    2468 kB (12%) ggc
   thread pro- & epilogue  :   0.01 ( 1%) usr   0.00 ( 0%) sys   0.00 ( 0%) wall     162 kB ( 1%) ggc
   final                   :   0.01 ( 1%) usr   0.00 ( 0%) sys   0.00 ( 0%) wall     216 kB ( 1%) ggc
   rest of compilation     :   1.37 (69%) usr   0.00 ( 0%) sys   1.13 (18%) wall    1391 kB ( 6%) ggc
   assemble JIT code       :   0.01 ( 1%) usr   0.00 ( 0%) sys   4.04 (66%) wall       0 kB ( 0%) ggc
   load JIT result         :   0.02 ( 1%) usr   0.00 ( 0%) sys   0.00 ( 0%) wall       0 kB ( 0%) ggc
   JIT client code         :   0.00 ( 0%) usr   0.01 ( 6%) sys   0.00 ( 0%) wall       0 kB ( 0%) ggc
  Client items:
   create_code             :   0.00 ( 0%) usr   0.01 ( 6%) sys   0.00 ( 0%) wall       0 kB ( 0%) ggc
   compile                 :   0.36 (18%) usr   0.15 (83%) sys   0.86 (14%) wall   14939 kB (70%) ggc
   running code            :   0.00 ( 0%) usr   0.00 ( 0%) sys   0.00 ( 0%) wall       0 kB ( 0%) ggc
   TOTAL                   :   2.00             0.18             6.12              21444 kB

The exact format is intended to be human-readable, and is subject to change.

.. macro:: LIBGCCJIT_HAVE_TIMING_API

   The timer API was added to libgccjit in GCC 6.
   This macro is only defined in versions of libgccjit.h which have the
   timer API, and so can be used to guard code that may need to compile
   against earlier releases::

     #ifdef LIBGCCJIT_HAVE_TIMING_API
     gcc_jit_timer *t = gcc_jit_timer_new ();
     gcc_jit_context_set_timer (ctxt, t);
     #endif

.. type:: gcc_jit_timer

.. function:: gcc_jit_timer * gcc_jit_timer_new(void)

   Create a :c:type:`gcc_jit_timer` instance, and start timing::

     gcc_jit_timer *t = gcc_jit_timer_new ();

   This API entrypoint was added in :ref:`LIBGCCJIT_ABI_4`; you can test
   for its presence using

   .. code-block:: c

     #ifdef LIBGCCJIT_HAVE_TIMING_API

.. function:: void gcc_jit_timer_release(gcc_jit_timer *timer)

   Release a :c:type:`gcc_jit_timer` instance::

     gcc_jit_timer_release (t);

   This should be called exactly once on a timer.

   This API entrypoint was added in :ref:`LIBGCCJIT_ABI_4`; you can test
   for its presence using

   .. code-block:: c

     #ifdef LIBGCCJIT_HAVE_TIMING_API

.. function:: void gcc_jit_context_set_timer(gcc_jit_context *ctxt, \
                                             gcc_jit_timer *timer)

   Associate a :c:type:`gcc_jit_timer` instance with a context::

      gcc_jit_context_set_timer (ctxt, t);

   A timer instance can be shared between multiple
   :c:type:`gcc_jit_context` instances.

   Timers have no locking, so if you have a multithreaded program, you
   must provide your own locks if more than one thread could be working
   with the same timer via timer-associated contexts.

   This API entrypoint was added in :ref:`LIBGCCJIT_ABI_4`; you can test
   for its presence using

   .. code-block:: c

     #ifdef LIBGCCJIT_HAVE_TIMING_API

.. function:: gcc_jit_timer *gcc_jit_context_get_timer(gcc_jit_context *ctxt)

   Get the timer associated with a context (if any).

   This API entrypoint was added in :ref:`LIBGCCJIT_ABI_4`; you can test
   for its presence using

   .. code-block:: c

     #ifdef LIBGCCJIT_HAVE_TIMING_API

.. function:: void gcc_jit_timer_push(gcc_jit_timer *timer, \
                                      const char *item_name)

   Push the given item onto the timer's stack::

      gcc_jit_timer_push (t, "running code");
      run_the_code (ctxt, result);
      gcc_jit_timer_pop (t, "running code");

   This API entrypoint was added in :ref:`LIBGCCJIT_ABI_4`; you can test
   for its presence using

   .. code-block:: c

     #ifdef LIBGCCJIT_HAVE_TIMING_API

.. function:: void gcc_jit_timer_pop(gcc_jit_timer *timer, \
                                     const char *item_name)

   Pop the top item from the timer's stack.

   If "item_name" is provided, it must match that of the top item.
   Alternatively, ``NULL`` can be passed in, to suppress checking.

   This API entrypoint was added in :ref:`LIBGCCJIT_ABI_4`; you can test
   for its presence using

   .. code-block:: c

     #ifdef LIBGCCJIT_HAVE_TIMING_API

.. function:: void gcc_jit_timer_print(gcc_jit_timer *timer, \
                                       FILE *f_out)

   Print timing information to the given stream about activity since
   the timer was started.

   This API entrypoint was added in :ref:`LIBGCCJIT_ABI_4`; you can test
   for its presence using

   .. code-block:: c

     #ifdef LIBGCCJIT_HAVE_TIMING_API
