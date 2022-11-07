..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: speed of compilation

.. _c++-modules:

C++ Modules
***********

Modules are a C++20 language feature.  As the name suggests, they
provides a modular compilation system, intending to provide both
faster builds and better library isolation.  The :P:`1103 <Merging Modules>`,
provides the easiest to read set
of changes to the standard, although it does not capture later
changes.

*G++'s modules support is not complete.*  Other than bugs, the
known missing pieces are:

*Private Module Fragment*
  The Private Module Fragment is recognized, but an error is emitted.

*Partition definition visibility rules*
  Entities may be defined in implementation partitions, and those
  definitions are not available outside of the module.  This is not
  implemented, and the definitions are available to extra-module use.

*Textual merging of reachable GM entities*
  Entities may be multiply defined across different header-units.
  These must be de-duplicated, and this is implemented across imports,
  or when an import redefines a textually-defined entity.  However the
  reverse is not implemented---textually redefining an entity that has
  been defined in an imported header-unit.  A redefinition error is
  emitted.

*Translation-Unit local referencing rules*
  Papers :P:`1815` and :P:`2003`
  add limitations on which entities an
  exported region may reference (for instance, the entities an exported
  template definition may reference).  These are not fully implemented.

*Standard Library Header Units*
  The Standard Library is not provided as importable header units.  If
  you want to import such units, you must explicitly build them first.
  If you do not do this with care, you may have multiple declarations,
  which the module machinery must merge---compiler resource usage can be
  affected by how you partition header files into header units.

Modular compilation is *not* enabled with just the
:option:`-std=c++20` option.  You must explicitly enable it with the
:option:`-fmodules-ts` option.  It is independent of the language
version selected, although in pre-C++20 versions, it is of course an
extension.

No new source file suffixes are required or supported.  If you wish to
use a non-standard suffix (see :ref:`overall-options`), you also need
to provide a :option:`-x c++` option too.Some users like to
distinguish module interface files with a new suffix, such as naming
the source ``module.cppm``, which involves
teaching all tools about the new suffix.  A different scheme, such as
naming ``module-m.cpp`` would be less invasive.

Compiling a module interface unit produces an additional output (to
the assembly or object file), called a Compiled Module Interface
(CMI).  This encodes the exported declarations of the module.
Importing a module reads in the CMI.  The import graph is a Directed
Acyclic Graph (DAG).  You must build imports before the importer.

Header files may themselves be compiled to header units, which are a
transitional ability aiming at faster compilation.  The
:option:`-fmodule-header` option is used to enable this, and implies
the :option:`-fmodules-ts` option.  These CMIs are named by the fully
resolved underlying header file, and thus may be a complete pathname
containing subdirectories.  If the header file is found at an absolute
pathname, the CMI location is still relative to a CMI root directory.

As header files often have no suffix, you commonly have to specify a
:option:`-x` option to tell the compiler the source is a header file.
You may use :option:`-x c++-header`, :option:`-x c++-user-header` or
:option:`-x c++-system-header`.  When used in conjunction with
:option:`-fmodules-ts`, these all imply an appropriate
:option:`-fmodule-header` option.  The latter two variants use the
user or system include path to search for the file specified.  This
allows you to, for instance, compile standard library header files as
header units, without needing to know exactly where they are
installed.  Specifying the language as one of these variants also
inhibits output of the object file, as header files have no associated
object file.

The :option:`-fmodule-only` option disables generation of the
associated object file for compiling a module interface.  Only the CMI
is generated.  This option is implied when using the
:option:`-fmodule-header` option.

The :option:`-flang-info-include-translate` and
:option:`-flang-info-include-translate-not` options notes whether
include translation occurs or not.  With no argument, the first will
note all include translation.  The second will note all
non-translations of include files not known to intentionally be
textual.  With an argument, queries about include translation of a
header files with that particular trailing pathname are noted.  You
may repeat this form to cover several different header files.  This
option may be helpful in determining whether include translation is
happening---if it is working correctly, it behaves as if it isn't
there at all.

The :option:`-flang-info-module-cmi` option can be used to determine
where the compiler is reading a CMI from.  Without the option, the
compiler is silent when such a read is successful.  This option has an
optional argument, which will restrict the notification to just the
set of named modules or header units specified.

The :option:`-Winvalid-imported-macros` option causes all imported macros
to be resolved at the end of compilation.  Without this, imported
macros are only resolved when expanded or (re)defined.  This option
detects conflicting import definitions for all macros.

For details of the :option:`-fmodule-mapper` family of options,
see :ref:`c++-module-mapper`.

.. toctree::
  :maxdepth: 2


.. index:: C++ Module Mapper

.. _c++-module-mapper:

Module Mapper
^^^^^^^^^^^^^

A module mapper provides a server or file that the compiler queries to
determine the mapping between module names and CMI files.  It is also
used to build CMIs on demand.  *Mapper functionality is in its
infancy and is intended for experimentation with build system
interactions.*

You can specify a mapper with the :option:`-fmodule-mapper=val`
option or :envvar:`CXX_MODULE_MAPPER` environment variable.  The value may
have one of the following forms:

:samp:`{[}{hostname}{]}:{port}{[}?{ident}{]}`
  An optional hostname and a numeric port number to connect to.  If the
  hostname is omitted, the loopback address is used.  If the hostname
  corresponds to multiple IPV6 addresses, these are tried in turn, until
  one is successful.  If your host lacks IPv6, this form is
  non-functional.  If you must use IPv4 use
  :option:`-fmodule-mapper='|ncat ipv4hostport'`.

:samp:`={socket}{[}?{ident}{]}`
  A local domain socket.  If your host lacks local domain sockets, this
  form is non-functional.

:samp:`|{program}{[}?{ident}{]}{[}{args...}{]}`
  A program to spawn, and communicate with on its stdin/stdout streams.
  Your :samp:`{PATH}` environment variable is searched for the program.
  Arguments are separated by space characters, (it is not possible for
  one of the arguments delivered to the program to contain a space).  An
  exception is if :samp:`{program}` begins with @.  In that case
  :samp:`{program}` (sans @) is looked for in the compiler's internal
  binary directory.  Thus the sample mapper-server can be specified
  with ``@g++-mapper-server``.

  :samp:`<>{[}?{ident}{]}`:samp:`<>{inout}{[}?{ident}{]}`
:samp:`<{in}>{out}{[}?{ident}{]}`
  Named pipes or file descriptors to communicate over.  The first form,
  <>, communicates over stdin and stdout.  The other forms
  allow you to specify a file descriptor or name a pipe.  A numeric value
  is interpreted as a file descriptor, otherwise named pipe is opened.
  The second form specifies a bidirectional pipe and the last form
  allows specifying two independent pipes.  Using file descriptors
  directly in this manner is fragile in general, as it can require the
  cooperation of intermediate processes.  In particular using stdin &
  stdout is fraught with danger as other compiler options might also
  cause the compiler to read stdin or write stdout, and it can have
  unfortunate interactions with signal delivery from the terminal.

:samp:`{file}{[}?{ident}{]}`
  A mapping file consisting of space-separated module-name, filename
  pairs, one per line.  Only the mappings for the direct imports and any
  module export name need be provided.  If other mappings are provided,
  they override those stored in any imported CMI files.  A repository
  root may be specified in the mapping file by using :samp:`$root` as the
  module name in the first active line.  Use of this option will disable
  any default module->CMI name mapping.

As shown, an optional :samp:`{ident}` may suffix the first word of the
option, indicated by a :samp:`?` prefix.  The value is used in the
initial handshake with the module server, or to specify a prefix on
mapping file lines.  In the server case, the main source file name is
used if no :samp:`{ident}` is specified.  In the file case, all non-blank
lines are significant, unless a value is specified, in which case only
lines beginning with :samp:`{ident}` are significant.  The :samp:`{ident}`
must be separated by whitespace from the module name.  Be aware that
:samp:`<`, :samp:`>`, :samp:`?`, and :samp:`|` characters are often
significant to the shell, and therefore may need quoting.

The mapper is connected to or loaded lazily, when the first module
mapping is required.  The networking protocols are only supported on
hosts that provide networking.  If no mapper is specified a default is
provided.

A project-specific mapper is expected to be provided by the build
system that invokes the compiler.  It is not expected that a
general-purpose server is provided for all compilations.  As such, the
server will know the build configuration, the compiler it invoked, and
the environment (such as working directory) in which that is
operating.  As it may parallelize builds, several compilations may
connect to the same socket.

The default mapper generates CMI files in a :samp:`gcm.cache`
directory.  CMI files have a :samp:`.gcm` suffix.  The module unit name
is used directly to provide the basename.  Header units construct a
relative path using the underlying header file name.  If the path is
already relative, a :samp:`,` directory is prepended.  Internal
:samp:`..` components are translated to :samp:`,,`.  No attempt is made
to canonicalize these filenames beyond that done by the preprocessor's
include search algorithm, as in general it is ambiguous when symbolic
links are present.

The mapper protocol was published as :P:`A Module Mapper <1184>`.
The implementation is provided by
:command:`libcody`, https://github.com/urnathan/libcody,
which specifies the canonical protocol definition.  A proof of concept
server implementation embedded in :command:`make` was described in
:P:`Make Me A Module <1602>`.

.. index:: C++ Module Preprocessing

.. _c++-module-preprocessing:

Module Preprocessing
^^^^^^^^^^^^^^^^^^^^

Modules affect preprocessing because of header units and include
translation.  Some uses of the preprocessor as a separate step either
do not produce a correct output, or require CMIs to be available.

Header units import macros.  These macros can affect later conditional
inclusion, which therefore can cascade to differing import sets.  When
preprocessing, it is necessary to load the CMI.  If a header unit is
unavailable, the preprocessor issues a warning and continue (when
not just preprocessing, an error is emitted).  Detecting such imports
requires preprocessor tokenization of the input stream to phase 4
(macro expansion).

Include translation converts ``#include``, ``#include_next`` and
``#import`` directives to internal ``import`` declarations.
Whether a particular directive is translated is controlled by the
module mapper.  Header unit names are canonicalized during
preprocessing.

Dependency information can be emitted for macro import, extending the
functionality of :option:`-MD` and :option:`-MMD` options.  Detection of
import declarations also requires phase 4 preprocessing, and thus
requires full preprocessing (or compilation).

The :option:`-M`, :option:`-MM` and :option:`-E -fdirectives-only` options halt
preprocessing before phase 4.

The :option:`-save-temps` option uses :option:`-fdirectives-only` for
preprocessing, and preserve the macro definitions in the preprocessed
output.  Usually you also want to use this option when explicitly
preprocessing a header-unit, or consuming such preprocessed output:

.. code-block:: c++

  g++ -fmodules-ts -E -fdirectives-only my-header.hh -o my-header.ii
  g++ -x c++-header -fmodules-ts -fpreprocessed -fdirectives-only my-header.ii

.. index:: C++ Compiled Module Interface

.. _c++-compiled-module-interface:

Compiled Module Interface
^^^^^^^^^^^^^^^^^^^^^^^^^

CMIs are an additional artifact when compiling named module
interfaces, partitions or header units.  These are read when
importing.  CMI contents are implementation-specific, and in GCC's
case tied to the compiler version.  Consider them a rebuildable cache
artifact, not a distributable object.

When creating an output CMI, any missing directory components are
created in a manner that is safe for concurrent builds creating
multiple, different, CMIs within a common subdirectory tree.

CMI contents are written to a temporary file, which is then atomically
renamed.  Observers either see old contents (if there is an
existing file), or complete new contents.  They do not observe the
CMI during its creation.  This is unlike object file writing, which
may be observed by an external process.

CMIs are read in lazily, if the host OS provides ``mmap``
functionality.  Generally blocks are read when name lookup or template
instantiation occurs.  To inhibit this, the :option:`-fno-module-lazy`
option may be used.

The :option:`--param lazy-modules=n` parameter controls the limit
on the number of concurrently open module files during lazy loading.
Should more modules be imported, an LRU algorithm is used to determine
which files to close---until that file is needed again.  This limit
may be exceeded with deep module dependency hierarchies.  With large
code bases there may be more imports than the process limit of file
descriptors.  By default, the limit is a few less than the per-process
file descriptor hard limit, if that is determinable.Where
applicable the soft limit is incremented as needed towards the hard limit.

GCC CMIs use ELF32 as an architecture-neutral encapsulation mechanism.
You may use :command:`readelf` to inspect them, although section
contents are largely undecipherable.  There is a section named
``.gnu.c++.README``, which contains human-readable text.  Other
than the first line, each line consists of ``tag: value``
tuples.

.. code-block:: shell-session

  $ readelf -p.gnu.c++.README gcm.cache/foo.gcm

  String dump of section '.gnu.c++.README':
    [     0]  GNU C++ primary module interface
    [    21]  compiler: 11.0.0 20201116 (experimental) [c++-modules revision 20201116-0454]
    [    6f]  version: 2020/11/16-04:54
    [    89]  module: foo
    [    95]  source: c_b.ii
    [    a4]  dialect: C++20/coroutines
    [    be]  cwd: /data/users/nathans/modules/obj/x86_64/gcc
    [    ee]  repository: gcm.cache
    [   104]  buildtime: 2020/11/16 15:03:21 UTC
    [   127]  localtime: 2020/11/16 07:03:21 PST
    [   14a]  export: foo:part1 foo-part1.gcm

Amongst other things, this lists the source that was built, C++
dialect used and imports of the module.The precise contents
of this output may change.

The timestamp is the same value as that
provided by the ``__DATE__`` & ``__TIME__`` macros, and may be
explicitly specified with the environment variable
``SOURCE_DATE_EPOCH``.  For further details
see :ref:`environment-variables`.

A set of related CMIs may be copied, provided the relative pathnames
are preserved.

The ``.gnu.c++.README`` contents do not affect CMI integrity, and
it may be removed or altered.  The section numbering of the sections
whose names do not begin with ``.gnu.c++.``, or are not the string
section is significant and must not be altered.
