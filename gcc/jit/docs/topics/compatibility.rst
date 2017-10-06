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

ABI and API compatibility
=========================

The libgccjit developers strive for ABI and API backward-compatibility:
programs built against libgccjit.so stand a good chance of running
without recompilation against newer versions of libgccjit.so, and
ought to recompile without modification against newer versions of
libgccjit.h.

.. note:: The libgccjit++.h C++ API is more experimental, and less
          locked-down at this time.

API compatibility is achieved by extending the API rather than changing
it.  For ABI compatiblity, we avoid bumping the SONAME, and instead use
symbol versioning to tag each symbol, so that a binary linked against
libgccjit.so is tagged according to the symbols that it uses.

For example, :func:`gcc_jit_context_add_command_line_option` was added in
``LIBGCCJIT_ABI_1``.  If a client program uses it, this can be detected
from metadata by using ``objdump``:

.. code-block:: bash

   $ objdump -p testsuite/jit/test-extra-options.c.exe | tail -n 8

   Version References:
     required from libgccjit.so.0:
       0x00824161 0x00 04 LIBGCCJIT_ABI_1
       0x00824160 0x00 03 LIBGCCJIT_ABI_0
     required from libc.so.6:

You can see the symbol tags provided by libgccjit.so using ``objdump``:

.. code-block:: bash

   $ objdump -p libgccjit.so | less
   [...snip...]
   Version definitions:
   1 0x01 0x0ff81f20 libgccjit.so.0
   2 0x00 0x00824160 LIBGCCJIT_ABI_0
   3 0x00 0x00824161 LIBGCCJIT_ABI_1
           LIBGCCJIT_ABI_0
   [...snip...]

ABI symbol tags
***************

The initial release of libgccjit (in gcc 5.1) did not use symbol versioning.

Newer releases use the following tags.

.. _LIBGCCJIT_ABI_0:

``LIBGCCJIT_ABI_0``
-------------------

All entrypoints in the initial release of libgccjit are tagged with
``LIBGCCJIT_ABI_0``, to signify the transition to symbol versioning.

Binaries built against older copies of ``libgccjit.so`` should
continue to work, with this being handled transparently by the linker
(see `this post
<https://gcc.gnu.org/ml/gcc-patches/2015-06/msg02126.html>`_)

.. _LIBGCCJIT_ABI_1:

``LIBGCCJIT_ABI_1``
-------------------
``LIBGCCJIT_ABI_1`` covers the addition of
:func:`gcc_jit_context_add_command_line_option`

.. _LIBGCCJIT_ABI_2:

``LIBGCCJIT_ABI_2``
-------------------
``LIBGCCJIT_ABI_2`` covers the addition of
:func:`gcc_jit_context_set_bool_allow_unreachable_blocks`

.. _LIBGCCJIT_ABI_3:

``LIBGCCJIT_ABI_3``
-------------------
``LIBGCCJIT_ABI_3`` covers the addition of switch statements via API
entrypoints:

  * :func:`gcc_jit_block_end_with_switch`

  * :func:`gcc_jit_case_as_object`

  * :func:`gcc_jit_context_new_case`

.. _LIBGCCJIT_ABI_4:

``LIBGCCJIT_ABI_4``
-------------------
``LIBGCCJIT_ABI_4`` covers the addition of timers via API
entrypoints:

  * :func:`gcc_jit_context_get_timer`

  * :func:`gcc_jit_context_set_timer`

  * :func:`gcc_jit_timer_new`

  * :func:`gcc_jit_timer_release`

  * :func:`gcc_jit_timer_push`

  * :func:`gcc_jit_timer_pop`

  * :func:`gcc_jit_timer_print`

.. _LIBGCCJIT_ABI_5:

``LIBGCCJIT_ABI_5``
-------------------
``LIBGCCJIT_ABI_5`` covers the addition of
:func:`gcc_jit_context_set_bool_use_external_driver`

.. _LIBGCCJIT_ABI_6:

``LIBGCCJIT_ABI_6``
-------------------
``LIBGCCJIT_ABI_6`` covers the addition of
:func:`gcc_jit_rvalue_set_bool_require_tail_call`

.. _LIBGCCJIT_ABI_7:

``LIBGCCJIT_ABI_7``
-------------------
``LIBGCCJIT_ABI_7`` covers the addition of
:func:`gcc_jit_type_get_aligned`

.. _LIBGCCJIT_ABI_8:

``LIBGCCJIT_ABI_8``
-------------------
``LIBGCCJIT_ABI_8`` covers the addition of
:func:`gcc_jit_type_get_vector`

.. _LIBGCCJIT_ABI_9:

``LIBGCCJIT_ABI_9``
-------------------
``LIBGCCJIT_ABI_9`` covers the addition of
:func:`gcc_jit_function_get_address`

.. _LIBGCCJIT_ABI_10:

``LIBGCCJIT_ABI_10``
--------------------

``LIBGCCJIT_ABI_10`` covers the addition of
:func:`gcc_jit_context_new_rvalue_from_vector`
