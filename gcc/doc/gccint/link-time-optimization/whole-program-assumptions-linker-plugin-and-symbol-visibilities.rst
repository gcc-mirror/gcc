..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _whopr:

Whole program assumptions, linker plugin and symbol visibilities
****************************************************************

Link-time optimization gives relatively minor benefits when used
alone.  The problem is that propagation of inter-procedural
information does not work well across functions and variables
that are called or referenced by other compilation units (such as
from a dynamically linked library).  We say that such functions
and variables are *externally visible*.

To make the situation even more difficult, many applications
organize themselves as a set of shared libraries, and the default
ELF visibility rules allow one to overwrite any externally
visible symbol with a different symbol at runtime.  This
basically disables any optimizations across such functions and
variables, because the compiler cannot be sure that the function
body it is seeing is the same function body that will be used at
runtime.  Any function or variable not declared ``static`` in
the sources degrades the quality of inter-procedural
optimization.

To avoid this problem the compiler must assume that it sees the
whole program when doing link-time optimization.  Strictly
speaking, the whole program is rarely visible even at link-time.
Standard system libraries are usually linked dynamically or not
provided with the link-time information.  In GCC, the whole
program option (:option:`-fwhole-program`) asserts that every
function and variable defined in the current compilation
unit is static, except for function ``main`` (note: at
link time, the current unit is the union of all objects compiled
with LTO).  Since some functions and variables need to
be referenced externally, for example by another DSO or from an
assembler file, GCC also provides the function and variable
attribute ``externally_visible`` which can be used to disable
the effect of :option:`-fwhole-program` on a specific symbol.

The whole program mode assumptions are slightly more complex in
C++, where inline functions in headers are put into *COMDAT*
sections.  COMDAT function and variables can be defined by
multiple object files and their bodies are unified at link-time
and dynamic link-time.  COMDAT functions are changed to local only
when their address is not taken and thus un-sharing them with a
library is not harmful.  COMDAT variables always remain externally
visible, however for readonly variables it is assumed that their
initializers cannot be overwritten by a different value.

GCC provides the function and variable attribute
``visibility`` that can be used to specify the visibility of
externally visible symbols (or alternatively an
:option:`-fdefault-visibility` command line option).  ELF defines
the ``default``, ``protected``, ``hidden`` and
``internal`` visibilities.

The most commonly used is visibility is ``hidden``.  It
specifies that the symbol cannot be referenced from outside of
the current shared library.  Unfortunately, this information
cannot be used directly by the link-time optimization in the
compiler since the whole shared library also might contain
non-LTO objects and those are not visible to the compiler.

GCC solves this problem using linker plugins.  A *linker
plugin* is an interface to the linker that allows an external
program to claim the ownership of a given object file.  The linker
then performs the linking procedure by querying the plugin about
the symbol table of the claimed objects and once the linking
decisions are complete, the plugin is allowed to provide the
final object file before the actual linking is made.  The linker
plugin obtains the symbol resolution information which specifies
which symbols provided by the claimed objects are bound from the
rest of a binary being linked.

GCC is designed to be independent of the rest of the toolchain
and aims to support linkers without plugin support.  For this
reason it does not use the linker plugin by default.  Instead,
the object files are examined by :command:`collect2` before being
passed to the linker and objects found to have LTO sections are
passed to :command:`lto1` first.  This mode does not work for
library archives.  The decision on what object files from the
archive are needed depends on the actual linking and thus GCC
would have to implement the linker itself.  The resolution
information is missing too and thus GCC needs to make an educated
guess based on :option:`-fwhole-program`.  Without the linker
plugin GCC also assumes that symbols are declared ``hidden``
and not referred by non-LTO code by default.
