..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _freestanding-environments:

Profiling and Test Coverage in Freestanding Environments
********************************************************

In case your application runs in a hosted environment such as GNU/Linux, then
this section is likely not relevant to you.  This section is intended for
application developers targeting freestanding environments (for example
embedded systems) with limited resources.  In particular, systems or test cases
which do not support constructors/destructors or the C library file I/O.  In
this section, the :dfn:`target system` runs your application instrumented for
profiling or test coverage.  You develop and analyze your application on the
:dfn:`host system`.  We now provide an overview how profiling and test coverage
can be obtained in this scenario followed by a tutorial which can be exercised
on the host system.  Finally, some system initialization caveats are listed.

Overview
^^^^^^^^

For an application instrumented for profiling or test coverage, the compiler
generates some global data structures which are updated by instrumentation code
while the application runs.  These data structures are called the :dfn:`gcov
information`.  Normally, when the application exits, the gcov information is
stored to :samp:`.gcda` files.  There is one file per translation unit
instrumented for profiling or test coverage.  The function
``__gcov_exit()``, which stores the gcov information to a file, is called by
a global destructor function for each translation unit instrumented for
profiling or test coverage.  It runs at process exit.  In a global constructor
function, the ``__gcov_init()`` function is called to register the gcov
information of a translation unit in a global list.  In some situations, this
procedure does not work.  Firstly, if you want to profile the global
constructor or exit processing of an operating system, the compiler generated
functions may conflict with the test objectives.  Secondly, you may want to
test early parts of the system initialization or abnormal program behaviour
which do not allow a global constructor or exit processing.  Thirdly, you need
a filesystem to store the files.

The :option:`-fprofile-info-section` GCC option enables you to use profiling and
test coverage in freestanding environments.  This option disables the use of
global constructors and destructors for the gcov information.  Instead, a
pointer to the gcov information is stored in a special linker input section for
each translation unit which is compiled with this option.  By default, the
section name is ``.gcov_info``.  The gcov information is statically
initialized.  The pointers to the gcov information from all translation units
of an executable can be collected by the linker in a contiguous memory block.
For the GNU linker, the below linker script output section definition can be
used to achieve this:

.. code-block:: c++

    .gcov_info      :
    {
      PROVIDE (__gcov_info_start = .);
      KEEP (*(.gcov_info))
      PROVIDE (__gcov_info_end = .);
    }

The linker will provide two global symbols, ``__gcov_info_start`` and
``__gcov_info_end``, which define the start and end of the array of pointers
to gcov information blocks, respectively.  The ``KEEP ()`` directive is
required to prevent a garbage collection of the pointers.  They are not
directly referenced by anything in the executable.  The section may be placed
in a read-only memory area.

In order to transfer the profiling and test coverage data from the target to
the host system, the application has to provide a function to produce a
reliable in order byte stream from the target to the host.  The byte stream may
be compressed and encoded using error detection and correction codes to meet
application-specific requirements.  The GCC provided :samp:`libgcov` target
library provides two functions, ``__gcov_info_to_gcda()`` and
``__gcov_filename_to_gcfn()``, to generate a byte stream from a gcov
information bock.  The functions are declared in ``#include <gcov.h>``.  The
byte stream can be deserialized by the :command:`merge-stream` subcommand of the
:command:`gcov-tool` to create or update :samp:`.gcda` files in the host
filesystem for the instrumented application.

Tutorial
^^^^^^^^

This tutorial should be exercised on the host system.  We will build a program
instrumented for test coverage.  The program runs an application and dumps the
gcov information to :samp:`stderr` encoded as a printable character stream.  The
application simply decodes such character streams from :samp:`stdin` and writes
the decoded character stream to :samp:`stdout` (warning: this is binary data).
The decoded character stream is consumed by the :command:`merge-stream`
subcommand of the :command:`gcov-tool` to create or update the :samp:`.gcda`
files.

To get started, create an empty directory.  Change into the new directory.
Then you will create the following three files in this directory

* :samp:`app.h` - a header file included by :samp:`app.c` and :samp:`main.c`,

* :samp:`app.c` - a source file which contains an example application, and

* :samp:`main.c` - a source file which contains the program main function and code
  to dump the gcov information.

Firstly, create the header file :samp:`app.h` with the following content:

.. code-block:: c++

  static const unsigned char a = 'a';

  static inline unsigned char *
  encode (unsigned char c, unsigned char buf[2])
  {
    buf[0] = c % 16 + a;
    buf[1] = (c / 16) % 16 + a;
    return buf;
  }

  extern void application (void);

Secondly, create the source file :samp:`app.c` with the following content:

.. code-block:: c++

  #include "app.h"

  #include <stdio.h>

  /* The application reads a character stream encoded by encode() from stdin,
     decodes it, and writes the decoded characters to stdout.  Characters other
     than the 16 characters 'a' to 'p' are ignored.  */

  static int can_decode (unsigned char c)
  {
    return (unsigned char)(c - a) < 16;
  }

  void
  application (void)
  {
    int first = 1;
    int i;
    unsigned char c;

    while ((i = fgetc (stdin)) != EOF)
      {
        unsigned char x = (unsigned char)i;

        if (can_decode (x))
          {
            if (first)
              c = x - a;
            else
              fputc (c + 16 * (x - a), stdout);
            first = !first;
          }
        else
          first = 1;
      }
  }

Thirdly, create the source file :samp:`main.c` with the following content:

.. code-block:: c++

  #include "app.h"

  #include <gcov.h>
  #include <stdio.h>
  #include <stdlib.h>

  /* The start and end symbols are provided by the linker script.  We use the
     array notation to avoid issues with a potential small-data area.  */

  extern const struct gcov_info *const __gcov_info_start[];
  extern const struct gcov_info *const __gcov_info_end[];

  /* This function shall produce a reliable in order byte stream to transfer the
     gcov information from the target to the host system.  */

  static void
  dump (const void *d, unsigned n, void *arg)
  {
    (void)arg;
    const unsigned char *c = d;
    unsigned char buf[2];

    for (unsigned i = 0; i < n; ++i)
      fwrite (encode (c[i], buf), sizeof (buf), 1, stderr);
  }

  /* The filename is serialized to a gcfn data stream by the
     __gcov_filename_to_gcfn() function.  The gcfn data is used by the
     "merge-stream" subcommand of the "gcov-tool" to figure out the filename
     associated with the gcov information. */

  static void
  filename (const char *f, void *arg)
  {
    __gcov_filename_to_gcfn (f, dump, arg);
  }

  /* The __gcov_info_to_gcda() function may have to allocate memory under
     certain conditions.  Simply try it out if it is needed for your application
     or not.  */

  static void *
  allocate (unsigned length, void *arg)
  {
    (void)arg;
    return malloc (length);
  }

  /* Dump the gcov information of all translation units.  */

  static void
  dump_gcov_info (void)
  {
    const struct gcov_info *const *info = __gcov_info_start;
    const struct gcov_info *const *end = __gcov_info_end;

    /* Obfuscate variable to prevent compiler optimizations.  */
    __asm__ ("" : "+r" (info));

    while (info != end)
    {
      void *arg = NULL;
      __gcov_info_to_gcda (*info, filename, dump, allocate, arg);
      fputc ('\n', stderr);
      ++info;
    }
  }

  /* The main() function just runs the application and then dumps the gcov
     information to stderr.  */

  int
  main (void)
  {
    application ();
    dump_gcov_info ();
    return 0;
  }

If we compile :samp:`app.c` with test coverage and no extra profiling options,
then a global constructor (``_sub_I_00100_0`` here, it may have a different
name in your environment) and destructor (``_sub_D_00100_1``) is used to
register and dump the gcov information, respectively.  We also see undefined
references to ``__gcov_init`` and ``__gcov_exit`` :

.. code-block:: shell-session

  $ gcc --coverage -c app.c
  $ nm app.o
  0000000000000000 r a
  0000000000000030 T application
  0000000000000000 t can_decode
                   U fgetc
                   U fputc
  0000000000000000 b __gcov0.application
  0000000000000038 b __gcov0.can_decode
  0000000000000000 d __gcov_.application
  00000000000000c0 d __gcov_.can_decode
                   U __gcov_exit
                   U __gcov_init
                   U __gcov_merge_add
                   U stdin
                   U stdout
  0000000000000161 t _sub_D_00100_1
  0000000000000151 t _sub_I_00100_0

Compile :samp:`app.c` and :samp:`main.c` with test coverage and
:option:`-fprofile-info-section`.  Now, a read-only pointer size object is
present in the ``.gcov_info`` section and there are no undefined references
to ``__gcov_init`` and ``__gcov_exit`` :

.. code-block:: shell-session

  $ gcc --coverage -fprofile-info-section -c main.c
  $ gcc --coverage -fprofile-info-section -c app.c
  $ objdump -h app.o

  app.o:     file format elf64-x86-64

  Sections:
  Idx Name          Size      VMA               LMA               File off  Algn
    0 .text         00000151  0000000000000000  0000000000000000  00000040  2**0
                    CONTENTS, ALLOC, LOAD, RELOC, READONLY, CODE
    1 .data         00000100  0000000000000000  0000000000000000  000001a0  2**5
                    CONTENTS, ALLOC, LOAD, RELOC, DATA
    2 .bss          00000040  0000000000000000  0000000000000000  000002a0  2**5
                    ALLOC
    3 .rodata       0000003c  0000000000000000  0000000000000000  000002a0  2**3
                    CONTENTS, ALLOC, LOAD, READONLY, DATA
    4 .gcov_info    00000008  0000000000000000  0000000000000000  000002e0  2**3
                    CONTENTS, ALLOC, LOAD, RELOC, READONLY, DATA
    5 .comment      0000004e  0000000000000000  0000000000000000  000002e8  2**0
                    CONTENTS, READONLY
    6 .note.GNU-stack 00000000  0000000000000000  0000000000000000  00000336  2**0
                    CONTENTS, READONLY
    7 .eh_frame     00000058  0000000000000000  0000000000000000  00000338  2**3
                    CONTENTS, ALLOC, LOAD, RELOC, READONLY, DATA

We have to customize the program link procedure so that all the
``.gcov_info`` linker input sections are placed in a contiguous memory block
with a begin and end symbol.  Firstly, get the default linker script using the
following commands (we assume a GNU linker):

.. code-block:: shell-session

  $ ld --verbose | sed '1,/^===/d' | sed '/^===/d' > linkcmds

Secondly, open the file :samp:`linkcmds` with a text editor and place the linker
output section definition from the overview after the ``.rodata`` section
definition.  Link the program executable using the customized linker script:

.. code-block:: shell-session

  $ gcc --coverage main.o app.o -T linkcmds -Wl,-Map,app.map

In the linker map file :samp:`app.map`, we see that the linker placed the
read-only pointer size objects of our objects files :samp:`main.o` and
:samp:`app.o` into a contiguous memory block and provided the symbols
``__gcov_info_start`` and ``__gcov_info_end`` :

.. code-block:: shell-session

  $ grep -C 1 "\.gcov_info" app.map

  .gcov_info      0x0000000000403ac0       0x10
                  0x0000000000403ac0                PROVIDE (__gcov_info_start = .)
   *(.gcov_info)
   .gcov_info     0x0000000000403ac0        0x8 main.o
   .gcov_info     0x0000000000403ac8        0x8 app.o
                  0x0000000000403ad0                PROVIDE (__gcov_info_end = .)

Make sure no :samp:`.gcda` files are present.  Run the program with nothing to
decode and dump :samp:`stderr` to the file :samp:`gcda-0.txt` (first run).  Run
the program to decode :samp:`gcda-0.txt` and send it to the :command:`gcov-tool`
using the :command:`merge-stream` subcommand to create the :samp:`.gcda` files
(second run).  Run :command:`gcov` to produce a report for :samp:`app.c`.  We see
that the first run with nothing to decode results in a partially covered
application:

.. code-block:: shell-session

  $ rm -f app.gcda main.gcda
  $ echo "" | ./a.out 2>gcda-0.txt
  $ ./a.out <gcda-0.txt 2>gcda-1.txt | gcov-tool merge-stream
  $ gcov -bc app.c
  File 'app.c'
  Lines executed:69.23% of 13
  Branches executed:66.67% of 6
  Taken at least once:50.00% of 6
  Calls executed:66.67% of 3
  Creating 'app.c.gcov'

  Lines executed:69.23% of 13

Run the program to decode :samp:`gcda-1.txt` and send it to the
:command:`gcov-tool` using the :command:`merge-stream` subcommand to update the
:samp:`.gcda` files.  Run :command:`gcov` to produce a report for :samp:`app.c`.
Since the second run decoded the gcov information of the first run, we have now
a fully covered application:

.. code-block:: shell-session

  $ ./a.out <gcda-1.txt 2>gcda-2.txt | gcov-tool merge-stream
  $ gcov -bc app.c
  File 'app.c'
  Lines executed:100.00% of 13
  Branches executed:100.00% of 6
  Taken at least once:100.00% of 6
  Calls executed:100.00% of 3
  Creating 'app.c.gcov'

  Lines executed:100.00% of 13

System Initialization Caveats
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The gcov information of a translation unit consists of several global data
structures.  For example, the instrumented code may update program flow graph
edge counters in a zero-initialized data structure.  It is safe to run
instrumented code before the zero-initialized data is cleared to zero.  The
coverage information obtained before the zero-initialized data is cleared to
zero is unusable.  Dumping the gcov information using
``__gcov_info_to_gcda()`` before the zero-initialized data is cleared to
zero or the initialized data is loaded, is undefined behaviour.  Clearing the
zero-initialized data to zero through a function instrumented for profiling or
test coverage is undefined behaviour, since it may produce inconsistent program
flow graph edge counters for example.