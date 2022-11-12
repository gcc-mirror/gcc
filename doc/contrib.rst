..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: contributors

.. _contributors:

Contributors to GCC
===================

The GCC project would like to thank its many contributors.  Without them the
project would not have been nearly as successful as it has been.  Any omissions
in this list are accidental.  Feel free to contact
jlaw@ventanamicro.com or gerald@pfeifer.com if you have been left
out or some of your contributions are not listed.  Please keep this list in
alphabetical order.

* Analog Devices helped implement the support for complex data types
  and iterators.

* John David Anglin for threading-related fixes and improvements to
  libstdc++-v3, and the HP-UX port.

* James van Artsdalen wrote the code that makes efficient use of
  the Intel 80387 register stack.

* Abramo and Roberto Bagnara for the SysV68 Motorola 3300 Delta Series
  port.

* Alasdair Baird for various bug fixes.

* Giovanni Bajo for analyzing lots of complicated C++ problem reports.

* Peter Barada for his work to improve code generation for new
  ColdFire cores.

* Gerald Baumgartner added the signature extension to the C++ front end.

* Godmar Back for his Java improvements and encouragement.

* Scott Bambrough for help porting the Java compiler.

* Wolfgang Bangerth for processing tons of bug reports.

* Jon Beniston for his Microsoft Windows port of Java and port to Lattice Mico32.

* Daniel Berlin for better DWARF 2 support, faster/better optimizations,
  improved alias analysis, plus migrating GCC to Bugzilla.

* Geoff Berry for his Java object serialization work and various patches.

* David Binderman tests weekly snapshots of GCC trunk against Fedora Rawhide
  for several architectures.

* Laurynas Biveinis for memory management work and DJGPP port fixes.

* Uros Bizjak for the implementation of x87 math built-in functions and
  for various middle end and i386 back end improvements and bug fixes.

* Eric Blake for helping to make GCJ and libgcj conform to the
  specifications.

* Janne Blomqvist for contributions to GNU Fortran.

* Hans-J. Boehm for his garbage collector, IA-64 libffi port, and other
  Java work.

* Segher Boessenkool for helping maintain the PowerPC port and the
  instruction combiner plus various contributions to the middle end.

* Neil Booth for work on cpplib, lang hooks, debug hooks and other
  miscellaneous clean-ups.

* Steven Bosscher for integrating the GNU Fortran front end into GCC and for
  contributing to the tree-ssa branch.

* Eric Botcazou for fixing middle- and backend bugs left and right.

* Per Bothner for his direction via the steering committee and various
  improvements to the infrastructure for supporting new languages.  Chill
  front end implementation.  Initial implementations of
  cpplib, fix-header, config.guess, libio, and past C++ library (libg++)
  maintainer.  Dreaming up, designing and implementing much of GCJ.

* Devon Bowen helped port GCC to the Tahoe.

* Don Bowman for mips-vxworks contributions.

* James Bowman for the FT32 port.

* Dave Brolley for work on cpplib and Chill.

* Paul Brook for work on the ARM architecture and maintaining GNU Fortran.

* Robert Brown implemented the support for Encore 32000 systems.

* Christian Bruel for improvements to local store elimination.

* Herman A.J. ten Brugge for various fixes.

* Joerg Brunsmann for Java compiler hacking and help with the GCJ FAQ.

* Joe Buck for his direction via the steering committee from its creation
  to 2013.

* Iain Buclaw for the D frontend.

* Craig Burley for leadership of the G77 Fortran effort.

* Tobias Burnus for contributions to GNU Fortran.

* Stephan Buys for contributing Doxygen notes for libstdc++.

* Paolo Carlini for libstdc++ work: lots of efficiency improvements to
  the C++ strings, streambufs and formatted I/O, hard detective work on
  the frustrating localization issues, and keeping up with the problem reports.

* John Carr for his alias work, SPARC hacking, infrastructure improvements,
  previous contributions to the steering committee, loop optimizations, etc.

* Stephane Carrez for 68HC11 and 68HC12 ports.

* Steve Chamberlain for support for the Renesas SH and H8 processors
  and the PicoJava processor, and for GCJ config fixes.

* Glenn Chambers for help with the GCJ FAQ.

* John-Marc Chandonia for various libgcj patches.

* Denis Chertykov for contributing and maintaining the AVR port, the first GCC port
  for an 8-bit architecture.

* Kito Cheng for his work on the RISC-V port, including bringing up the test
  suite and maintenance.

* Scott Christley for his Objective-C contributions.

* Eric Christopher for his Java porting help and clean-ups.

* Branko Cibej for more warning contributions.

* The `GNU Classpath project <https://www.gnu.org/software/classpath/>`_
  for all of their merged runtime code.

* Nick Clifton for arm, mcore, fr30, v850, m32r, msp430 rx work,
  :option:`--help`, and other random hacking.

* Michael Cook for libstdc++ cleanup patches to reduce warnings.

* R. Kelley Cook for making GCC buildable from a read-only directory as
  well as other miscellaneous build process and documentation clean-ups.

* Ralf Corsepius for SH testing and minor bug fixing.

* François-Xavier Coudert for contributions to GNU Fortran.

* Stan Cox for care and feeding of the x86 port and lots of behind
  the scenes hacking.

* Alex Crain provided changes for the 3b1.

* Ian Dall for major improvements to the NS32k port.

* Paul Dale for his work to add uClinux platform support to the
  m68k backend.

* Palmer Dabbelt for his work maintaining the RISC-V port.

* Dario Dariol contributed the four varieties of sample programs
  that print a copy of their source.

* Russell Davidson for fstream and stringstream fixes in libstdc++.

* Bud Davis for work on the G77 and GNU Fortran compilers.

* Mo DeJong for GCJ and libgcj bug fixes.

* Jerry DeLisle for contributions to GNU Fortran.

* DJ Delorie for the DJGPP port, build and libiberty maintenance,
  various bug fixes, and the M32C, MeP, MSP430, and RL78 ports.

* Arnaud Desitter for helping to debug GNU Fortran.

* Gabriel Dos Reis for contributions to G++, contributions and
  maintenance of GCC diagnostics infrastructure, libstdc++-v3,
  including ``valarray<>``, ``complex<>``, maintaining the numerics library
  (including that pesky ``<limits>`` :-) and keeping up-to-date anything
  to do with numbers.

* Ulrich Drepper for his work on glibc, testing of GCC using glibc, ISO C99
  support, CFG dumping support, etc., plus support of the C++ runtime
  libraries including for all kinds of C interface issues, contributing and
  maintaining ``complex<>``, sanity checking and disbursement, configuration
  architecture, libio maintenance, and early math work.

* François Dumont for his work on libstdc++-v3, especially maintaining and
  improving ``debug-mode`` and associative and unordered containers.

* Zdenek Dvorak for a new loop unroller and various fixes.

* Michael Eager for his work on the Xilinx MicroBlaze port.

* Richard Earnshaw for his ongoing work with the ARM.

* David Edelsohn for his direction via the steering committee, ongoing work
  with the RS6000/PowerPC port, help cleaning up Haifa loop changes,
  doing the entire AIX port of libstdc++ with his bare hands, and for
  ensuring GCC properly keeps working on AIX.

* Kevin Ediger for the floating point formatting of num_put::do_put in
  libstdc++.

* Phil Edwards for libstdc++ work including configuration hackery,
  documentation maintainer, chief breaker of the web pages, the occasional
  iostream bug fix, and work on shared library symbol versioning.

* Paul Eggert for random hacking all over GCC.

* Mark Elbrecht for various DJGPP improvements, and for libstdc++
  configuration support for locales and fstream-related fixes.

* Vadim Egorov for libstdc++ fixes in strings, streambufs, and iostreams.

* Christian Ehrhardt for dealing with bug reports.

* Ben Elliston for his work to move the Objective-C runtime into its
  own subdirectory and for his work on autoconf.

* Revital Eres for work on the PowerPC 750CL port.

* Marc Espie for OpenBSD support.

* Doug Evans for much of the global optimization framework, arc, m32r,
  and SPARC work.

* Christopher Faylor for his work on the Cygwin port and for caring and
  feeding the gcc.gnu.org box and saving its users tons of spam.

* Fred Fish for BeOS support and Ada fixes.

* Ivan Fontes Garcia for the Portuguese translation of the GCJ FAQ.

* Peter Gerwinski for various bug fixes and the Pascal front end.

* Kaveh R. Ghazi for his direction via the steering committee, amazing
  work to make :samp:`-W -Wall -W* -Werror` useful, and
  testing GCC on a plethora of platforms.  Kaveh extends his gratitude to
  the CAIP Center at Rutgers University for providing him with computing
  resources to work on Free Software from the late 1980s to 2010.

* John Gilmore for a donation to the FSF earmarked improving GNU Java.

* Judy Goldberg for c++ contributions.

* Torbjorn Granlund for various fixes and the c-torture testsuite,
  multiply- and divide-by-constant optimization, improved long long
  support, improved leaf function register allocation, and his direction
  via the steering committee.

* Jonny Grant for improvements to ``collect2's`` :option:`--help` documentation.

* Anthony Green for his :option:`-Os` contributions, the moxie port, and
  Java front end work.

* Stu Grossman for gdb hacking, allowing GCJ developers to debug Java code.

* Michael K. Gschwind contributed the port to the PDP-11.

* Richard Biener for his ongoing middle-end contributions and bug fixes
  and for release management.

* Ron Guilmette implemented the :command:`protoize` and :command:`unprotoize`
  tools, the support for DWARF 1 symbolic debugging information, and much of
  the support for System V Release 4.  He has also worked heavily on the
  Intel 386 and 860 support.

* Sumanth Gundapaneni for contributing the CR16 port.

* Mostafa Hagog for Swing Modulo Scheduling (SMS) and post reload GCSE.

* Bruno Haible for improvements in the runtime overhead for EH, new
  warnings and assorted bug fixes.

* Andrew Haley for his amazing Java compiler and library efforts.

* Chris Hanson assisted in making GCC work on HP-UX for the 9000 series 300.

* Michael Hayes for various thankless work he's done trying to get
  the c30/c40 ports functional.  Lots of loop and unroll improvements and
  fixes.

* Dara Hazeghi for wading through myriads of target-specific bug reports.

* Kate Hedstrom for staking the G77 folks with an initial testsuite.

* Richard Henderson for his ongoing SPARC, alpha, ia32, and ia64 work, loop
  opts, and generally fixing lots of old problems we've ignored for
  years, flow rewrite and lots of further stuff, including reviewing
  tons of patches.

* Aldy Hernandez for working on the PowerPC port, SIMD support, and
  various fixes.

* Nobuyuki Hikichi of Software Research Associates, Tokyo, contributed
  the support for the Sony NEWS machine.

* Kazu Hirata for caring and feeding the Renesas H8/300 port and various fixes.

* Katherine Holcomb for work on GNU Fortran.

* Manfred Hollstein for his ongoing work to keep the m88k alive, lots
  of testing and bug fixing, particularly of GCC configury code.

* Steve Holmgren for MachTen patches.

* Mat Hostetter for work on the TILE-Gx and TILEPro ports.

* Jan Hubicka for his x86 port improvements.

* Falk Hueffner for working on C and optimization bug reports.

* Bernardo Innocenti for his m68k work, including merging of
  ColdFire improvements and uClinux support.

* Christian Iseli for various bug fixes.

* Kamil Iskra for general m68k hacking.

* Lee Iverson for random fixes and MIPS testing.

* Balaji V. Iyer for Cilk+ development and merging.

* Andreas Jaeger for testing and benchmarking of GCC and various bug fixes.

* Martin Jambor for his work on inter-procedural optimizations, the
  switch conversion pass, and scalar replacement of aggregates.

* Jakub Jelinek for his SPARC work and sibling call optimizations as well
  as lots of bug fixes and test cases, and for improving the Java build
  system.

* Janis Johnson for ia64 testing and fixes, her quality improvement
  sidetracks, and web page maintenance.

* Kean Johnston for SCO OpenServer support and various fixes.

* Tim Josling for the sample language treelang based originally on Richard
  Kenner's 'toy' language.

* Nicolai Josuttis for additional libstdc++ documentation.

* Klaus Kaempf for his ongoing work to make alpha-vms a viable target.

* Steven G. Kargl for work on GNU Fortran.

* David Kashtan of SRI adapted GCC to VMS.

* Ryszard Kabatek for many, many libstdc++ bug fixes and optimizations of
  strings, especially member functions, and for auto_ptr fixes.

* Geoffrey Keating for his ongoing work to make the PPC work for GNU/Linux
  and his automatic regression tester.

* Brendan Kehoe for his ongoing work with G++ and for a lot of early work
  in just about every part of libstdc++.

* Oliver M. Kellogg of Deutsche Aerospace contributed the port to the
  MIL-STD-1750A.

* Richard Kenner of the New York University Ultracomputer Research
  Laboratory wrote the machine descriptions for the AMD 29000, the DEC
  Alpha, the IBM RT PC, and the IBM RS/6000 as well as the support for
  instruction attributes.  He also made changes to better support RISC
  processors including changes to common subexpression elimination,
  strength reduction, function calling sequence handling, and condition
  code support, in addition to generalizing the code for frame pointer
  elimination and delay slot scheduling.  Richard Kenner was also the
  head maintainer of GCC for several years.

* Mumit Khan for various contributions to the Cygwin and Mingw32 ports and
  maintaining binary releases for Microsoft Windows hosts, and for massive libstdc++
  porting work to Cygwin/Mingw32.

* Robin Kirkham for cpu32 support.

* Mark Klein for PA improvements.

* Thomas Koenig for various bug fixes.

* Bruce Korb for the new and improved fixincludes code.

* Benjamin Kosnik for his G++ work and for leading the libstdc++-v3 effort.

* Maxim Kuvyrkov for contributions to the instruction scheduler, the Android
  and m68k/Coldfire ports, and optimizations.

* Charles LaBrec contributed the support for the Integrated Solutions
  68020 system.

* Asher Langton and Mike Kumbera for contributing Cray pointer support
  to GNU Fortran, and for other GNU Fortran improvements.

* Jeff Law for his direction via the steering committee, coordinating the
  entire egcs project and GCC 2.95, rolling out snapshots and releases,
  handling merges from GCC2, reviewing tons of patches that might have
  fallen through the cracks else, and random but extensive hacking.

* Walter Lee for work on the TILE-Gx and TILEPro ports.

* Marc Lehmann for his direction via the steering committee and helping
  with analysis and improvements of x86 performance.

* Victor Leikehman for work on GNU Fortran.

* Ted Lemon wrote parts of the RTL reader and printer.

* Kriang Lerdsuwanakij for C++ improvements including template as template
  parameter support, and many C++ fixes.

* Warren Levy for tremendous work on libgcj (Java Runtime Library) and
  random work on the Java front end.

* Alain Lichnewsky ported GCC to the MIPS CPU.

* Oskar Liljeblad for hacking on AWT and his many Java bug reports and
  patches.

* Robert Lipe for OpenServer support, new testsuites, testing, etc.

* Chen Liqin for various S+core related fixes/improvement, and for
  maintaining the S+core port.

* Martin Liska for his work on identical code folding, the sanitizers,
  HSA, general bug fixing and for running automated regression testing of GCC
  and reporting numerous bugs.

* Weiwen Liu for testing and various bug fixes.

* Manuel López-Ibáñez for improving :option:`-Wconversion` and
  many other diagnostics fixes and improvements.

* Dave Love for his ongoing work with the Fortran front end and
  runtime libraries.

* Martin von Löwis for internal consistency checking infrastructure,
  various C++ improvements including namespace support, and tons of
  assistance with libstdc++/compiler merges.

* H.J. Lu for his previous contributions to the steering committee, many x86
  bug reports, prototype patches, and keeping the GNU/Linux ports working.

* Greg McGary for random fixes and (someday) bounded pointers.

* Andrew MacLeod for his ongoing work in building a real EH system,
  various code generation improvements, work on the global optimizer, etc.

* Vladimir Makarov for hacking some ugly i960 problems, PowerPC hacking
  improvements to compile-time performance, overall knowledge and
  direction in the area of instruction scheduling, design and
  implementation of the automaton based instruction scheduler and
  design and implementation of the integrated and local register allocators.

* David Malcolm for his work on improving GCC diagnostics, JIT, self-tests
  and unit testing.

* Bob Manson for his behind the scenes work on dejagnu.

* John Marino for contributing the DragonFly BSD port.

* Philip Martin for lots of libstdc++ string and vector iterator fixes and
  improvements, and string clean up and testsuites.

* Michael Matz for his work on dominance tree discovery, the x86-64 port,
  link-time optimization framework and general optimization improvements.

* All of the Mauve project contributors for Java test code.

* Bryce McKinlay for numerous GCJ and libgcj fixes and improvements.

* Adam Megacz for his work on the Microsoft Windows port of GCJ.

* Michael Meissner for LRS framework, ia32, m32r, v850, m88k, MIPS,
  powerpc, haifa, ECOFF debug support, and other assorted hacking.

* Jason Merrill for his direction via the steering committee and leading
  the G++ effort.

* Martin Michlmayr for testing GCC on several architectures using the
  entire Debian archive.

* David Miller for his direction via the steering committee, lots of
  SPARC work, improvements in jump.cc and interfacing with the Linux kernel
  developers.

* Gary Miller ported GCC to Charles River Data Systems machines.

* Alfred Minarik for libstdc++ string and ios bug fixes, and turning the
  entire libstdc++ testsuite namespace-compatible.

* Mark Mitchell for his direction via the steering committee, mountains of
  C++ work, load/store hoisting out of loops, alias analysis improvements,
  ISO C ``restrict`` support, and serving as release manager from 2000
  to 2011.

* Alan Modra for various GNU/Linux bits and testing.

* Toon Moene for his direction via the steering committee, Fortran
  maintenance, and his ongoing work to make us make Fortran run fast.

* Jason Molenda for major help in the care and feeding of all the services
  on the gcc.gnu.org (formerly egcs.cygnus.com) machine---mail, web
  services, ftp services, etc etc.  Doing all this work on scrap paper and
  the backs of envelopes would have been... difficult.

* Catherine Moore for fixing various ugly problems we have sent her
  way, including the haifa bug which was killing the Alpha & PowerPC
  Linux kernels.

* Mike Moreton for his various Java patches.

* David Mosberger-Tang for various Alpha improvements, and for the initial
  IA-64 port.

* Stephen Moshier contributed the floating point emulator that assists in
  cross-compilation and permits support for floating point numbers wider
  than 64 bits and for ISO C99 support.

* Bill Moyer for his behind the scenes work on various issues.

* Philippe De Muyter for his work on the m68k port.

* Joseph S. Myers for his work on the PDP-11 port, format checking and ISO
  C99 support, and continuous emphasis on (and contributions to) documentation.

* Nathan Myers for his work on libstdc++-v3: architecture and authorship
  through the first three snapshots, including implementation of locale
  infrastructure, string, shadow C headers, and the initial project
  documentation (DESIGN, CHECKLIST, and so forth).  Later, more work on
  MT-safe string and shadow headers.

* Felix Natter for documentation on porting libstdc++.

* Nathanael Nerode for cleaning up the configuration/build process.

* NeXT, Inc. donated the front end that supports the Objective-C
  language.

* Hans-Peter Nilsson for the CRIS and MMIX ports, improvements to the search
  engine setup, various documentation fixes and other small fixes.

* Geoff Noer for his work on getting cygwin native builds working.

* Vegard Nossum for running automated regression testing of GCC and reporting
  numerous bugs.

* Diego Novillo for his work on Tree SSA, OpenMP, SPEC performance
  tracking web pages, GIMPLE tuples, and assorted fixes.

* David O'Brien for the FreeBSD/alpha, FreeBSD/AMD x86-64, FreeBSD/ARM,
  FreeBSD/PowerPC, and FreeBSD/SPARC64 ports and related infrastructure
  improvements.

* Alexandre Oliva for various build infrastructure improvements, scripts and
  amazing testing work, including keeping libtool issues sane and happy.

* Stefan Olsson for work on mt_alloc.

* Melissa O'Neill for various NeXT fixes.

* Rainer Orth for random MIPS work, including improvements to GCC's o32
  ABI support, improvements to dejagnu's MIPS support, Java configuration
  clean-ups and porting work, and maintaining the IRIX, Solaris 2, and
  Tru64 UNIX ports.

* Steven Pemberton for his contribution of :samp:`enquire` which allowed GCC to
  determine various properties of the floating point unit and generate
  :samp:`float.h` in older versions of GCC.

* Hartmut Penner for work on the s390 port.

* Paul Petersen wrote the machine description for the Alliant FX/8.

* Alexandre Petit-Bianco for implementing much of the Java compiler and
  continued Java maintainership.

* Matthias Pfaller for major improvements to the NS32k port.

* Gerald Pfeifer for his direction via the steering committee, pointing
  out lots of problems we need to solve, maintenance of the web pages, and
  taking care of documentation maintenance in general.

* Marek Polacek for his work on the C front end, the sanitizers and general
  bug fixing.

* Andrew Pinski for processing bug reports by the dozen.

* Ovidiu Predescu for his work on the Objective-C front end and runtime
  libraries.

* Jerry Quinn for major performance improvements in C++ formatted I/O.

* Ken Raeburn for various improvements to checker, MIPS ports and various
  cleanups in the compiler.

* Rolf W. Rasmussen for hacking on AWT.

* David Reese of Sun Microsystems contributed to the Solaris on PowerPC
  port.

* John Regehr for running automated regression testing of GCC and reporting
  numerous bugs.

* Volker Reichelt for running automated regression testing of GCC and reporting
  numerous bugs and for keeping up with the problem reports.

* Joern Rennecke for maintaining the sh port, loop, regmove & reload
  hacking and developing and maintaining the Epiphany port.

* Loren J. Rittle for improvements to libstdc++-v3 including the FreeBSD
  port, threading fixes, thread-related configury changes, critical
  threading documentation, and solutions to really tricky I/O problems,
  as well as keeping GCC properly working on FreeBSD and continuous testing.

* Craig Rodrigues for processing tons of bug reports.

* Ola Rönnerup for work on mt_alloc.

* Gavin Romig-Koch for lots of behind the scenes MIPS work.

* David Ronis inspired and encouraged Craig to rewrite the G77
  documentation in texinfo format by contributing a first pass at a
  translation of the old :samp:`g77-0.5.16/f/DOC` file.

* Ken Rose for fixes to GCC's delay slot filling code.

* Ira Rosen for her contributions to the auto-vectorizer.

* Paul Rubin wrote most of the preprocessor.

* Pétur Runólfsson for major performance improvements in C++ formatted I/O and
  large file support in C++ filebuf.

* Chip Salzenberg for libstdc++ patches and improvements to locales, traits,
  Makefiles, libio, libtool hackery, and 'long long' support.

* Juha Sarlin for improvements to the H8 code generator.

* Greg Satz assisted in making GCC work on HP-UX for the 9000 series 300.

* Roger Sayle for improvements to constant folding and GCC's RTL optimizers
  as well as for fixing numerous bugs.

* Bradley Schatz for his work on the GCJ FAQ.

* Peter Schauer wrote the code to allow debugging to work on the Alpha.

* William Schelter did most of the work on the Intel 80386 support.

* Tobias Schlüter for work on GNU Fortran.

* Bernd Schmidt for various code generation improvements and major
  work in the reload pass, serving as release manager for
  GCC 2.95.3, and work on the Blackfin and C6X ports.

* Peter Schmid for constant testing of libstdc++---especially application
  testing, going above and beyond what was requested for the release
  criteria---and libstdc++ header file tweaks.

* Jason Schroeder for jcf-dump patches.

* Andreas Schwab for his work on the m68k port.

* Lars Segerlund for work on GNU Fortran.

* Dodji Seketeli for numerous C++ bug fixes and debug info improvements.

* Tim Shen for major work on ``<regex>``.

* Joel Sherrill for his direction via the steering committee, RTEMS
  contributions and RTEMS testing.

* Nathan Sidwell for many C++ fixes/improvements.

* Jeffrey Siegal for helping RMS with the original design of GCC, some
  code which handles the parse tree and RTL data structures, constant
  folding and help with the original VAX & m68k ports.

* Kenny Simpson for prompting libstdc++ fixes due to defect reports from
  the LWG (thereby keeping GCC in line with updates from the ISO).

* Franz Sirl for his ongoing work with making the PPC port stable
  for GNU/Linux.

* Andrey Slepuhin for assorted AIX hacking.

* Trevor Smigiel for contributing the SPU port.

* Christopher Smith did the port for Convex machines.

* Danny Smith for his major efforts on the Mingw (and Cygwin) ports.
  Retired from GCC maintainership August 2010, having mentored two
  new maintainers into the role.

* Randy Smith finished the Sun FPA support.

* Ed Smith-Rowland for his continuous work on libstdc++-v3, special functions,
  ``<random>``, and various improvements to C++11 features.

* Scott Snyder for queue, iterator, istream, and string fixes and libstdc++
  testsuite entries.  Also for providing the patch to G77 to add
  rudimentary support for ``INTEGER*1``, ``INTEGER*2``, and
  ``LOGICAL*1``.

* Zdenek Sojka for running automated regression testing of GCC and reporting
  numerous bugs.

* Arseny Solokha for running automated regression testing of GCC and reporting
  numerous bugs.

* Jayant Sonar for contributing the CR16 port.

* Brad Spencer for contributions to the GLIBCPP_FORCE_NEW technique.

* Richard Stallman, for writing the original GCC and launching the GNU project.

* Jan Stein of the Chalmers Computer Society provided support for
  Genix, as well as part of the 32000 machine description.

* Gerhard Steinmetz for running automated regression testing of GCC and reporting
  numerous bugs.

* Nigel Stephens for various mips16 related fixes/improvements.

* Jonathan Stone wrote the machine description for the Pyramid computer.

* Graham Stott for various infrastructure improvements.

* John Stracke for his Java HTTP protocol fixes.

* Mike Stump for his Elxsi port, G++ contributions over the years and more
  recently his vxworks contributions

* Jeff Sturm for Java porting help, bug fixes, and encouragement.

* Zhendong Su for running automated regression testing of GCC and reporting
  numerous bugs.

* Chengnian Sun for running automated regression testing of GCC and reporting
  numerous bugs.

* Shigeya Suzuki for this fixes for the bsdi platforms.

* Ian Lance Taylor for the Go frontend, the initial mips16 and mips64
  support, general configury hacking, fixincludes, etc.

* Holger Teutsch provided the support for the Clipper CPU.

* Gary Thomas for his ongoing work to make the PPC work for GNU/Linux.

* Paul Thomas for contributions to GNU Fortran.

* Philipp Thomas for random bug fixes throughout the compiler

* Jason Thorpe for thread support in libstdc++ on NetBSD.

* Kresten Krab Thorup wrote the run time support for the Objective-C
  language and the fantastic Java bytecode interpreter.

* Michael Tiemann for random bug fixes, the first instruction scheduler,
  initial C++ support, function integration, NS32k, SPARC and M88k
  machine description work, delay slot scheduling.

* Andreas Tobler for his work porting libgcj to Darwin.

* Teemu Torma for thread safe exception handling support.

* Leonard Tower wrote parts of the parser, RTL generator, and RTL
  definitions, and of the VAX machine description.

* Daniel Towner and Hariharan Sandanagobalane contributed and
  maintain the picoChip port.

* Tom Tromey for internationalization support and for his many Java
  contributions and libgcj maintainership.

* Lassi Tuura for improvements to config.guess to determine HP processor
  types.

* Petter Urkedal for libstdc++ CXXFLAGS, math, and algorithms fixes.

* Andy Vaught for the design and initial implementation of the GNU Fortran
  front end.

* Brent Verner for work with the libstdc++ cshadow files and their
  associated configure steps.

* Todd Vierling for contributions for NetBSD ports.

* Andrew Waterman for contributing the RISC-V port, as well as maintaining it.

* Jonathan Wakely for contributing libstdc++ Doxygen notes and XHTML
  guidance and maintaining libstdc++.

* Dean Wakerley for converting the install documentation from HTML to texinfo
  in time for GCC 3.0.

* Krister Walfridsson for random bug fixes.

* Feng Wang for contributions to GNU Fortran.

* Stephen M. Webb for time and effort on making libstdc++ shadow files
  work with the tricky Solaris 8+ headers, and for pushing the build-time
  header tree. Also, for starting and driving the ``<regex>`` effort.

* John Wehle for various improvements for the x86 code generator,
  related infrastructure improvements to help x86 code generation,
  value range propagation and other work, WE32k port.

* Ulrich Weigand for work on the s390 port.

* Janus Weil for contributions to GNU Fortran.

* Zack Weinberg for major work on cpplib and various other bug fixes.

* Matt Welsh for help with Linux Threads support in GCJ.

* Urban Widmark for help fixing java.io.

* Mark Wielaard for new Java library code and his work integrating with
  Classpath.

* Dale Wiles helped port GCC to the Tahoe.

* Bob Wilson from Tensilica, Inc. for the Xtensa port.

* Jim Wilson for his direction via the steering committee, tackling hard
  problems in various places that nobody else wanted to work on, strength
  reduction and other loop optimizations.

* Paul Woegerer and Tal Agmon for the CRX port.

* Carlo Wood for various fixes.

* Tom Wood for work on the m88k port.

* Chung-Ju Wu for his work on the Andes NDS32 port.

* Canqun Yang for work on GNU Fortran.

* Masanobu Yuhara of Fujitsu Laboratories implemented the machine
  description for the Tron architecture (specifically, the Gmicro).

* Kevin Zachmann helped port GCC to the Tahoe.

* Ayal Zaks for Swing Modulo Scheduling (SMS).

* Qirun Zhang for running automated regression testing of GCC and reporting
  numerous bugs.

* Xiaoqiang Zhang for work on GNU Fortran.

* Gilles Zunino for help porting Java to Irix.

The following people are recognized for their contributions to GNAT,
the Ada front end of GCC:

* Bernard Banner

* Romain Berrendonner

* Geert Bosch

* Emmanuel Briot

* Joel Brobecker

* Ben Brosgol

* Vincent Celier

* Arnaud Charlet

* Chien Chieng

* Cyrille Comar

* Cyrille Crozes

* Robert Dewar

* Gary Dismukes

* Robert Duff

* Ed Falis

* Ramon Fernandez

* Sam Figueroa

* Vasiliy Fofanov

* Michael Friess

* Franco Gasperoni

* Ted Giering

* Matthew Gingell

* Laurent Guerby

* Jerome Guitton

* Olivier Hainque

* Jerome Hugues

* Hristian Kirtchev

* Jerome Lambourg

* Bruno Leclerc

* Albert Lee

* Sean McNeil

* Javier Miranda

* Laurent Nana

* Pascal Obry

* Dong-Ik Oh

* Laurent Pautet

* Brett Porter

* Thomas Quinot

* Nicolas Roche

* Pat Rogers

* Jose Ruiz

* Douglas Rupp

* Sergey Rybin

* Gail Schenker

* Ed Schonberg

* Nicolas Setton

* Samuel Tardieu

The following people are recognized for their contributions of new
features, bug reports, testing and integration of classpath/libgcj for
GCC version 4.1:

* Lillian Angel for ``JTree`` implementation and lots Free Swing
  additions and bug fixes.

* Wolfgang Baer for ``GapContent`` bug fixes.

* Anthony Balkissoon for ``JList``, Free Swing 1.5 updates and mouse event
  fixes, lots of Free Swing work including ``JTable`` editing.

* Stuart Ballard for RMI constant fixes.

* Goffredo Baroncelli for ``HTTPURLConnection`` fixes.

* Gary Benson for ``MessageFormat`` fixes.

* Daniel Bonniot for ``Serialization`` fixes.

* Chris Burdess for lots of gnu.xml and http protocol fixes, ``StAX``
  and ``DOM xml:id`` support.

* Ka-Hing Cheung for ``TreePath`` and ``TreeSelection`` fixes.

* Archie Cobbs for build fixes, VM interface updates,
  ``URLClassLoader`` updates.

* Kelley Cook for build fixes.

* Martin Cordova for Suggestions for better ``SocketTimeoutException``.

* David Daney for ``BitSet`` bug fixes, ``HttpURLConnection``
  rewrite and improvements.

* Thomas Fitzsimmons for lots of upgrades to the gtk+ AWT and Cairo 2D
  support. Lots of imageio framework additions, lots of AWT and Free
  Swing bug fixes.

* Jeroen Frijters for ``ClassLoader`` and nio cleanups, serialization fixes,
  better ``Proxy`` support, bug fixes and IKVM integration.

* Santiago Gala for ``AccessControlContext`` fixes.

* Nicolas Geoffray for ``VMClassLoader`` and ``AccessController``
  improvements.

* David Gilbert for ``basic`` and ``metal`` icon and plaf support
  and lots of documenting, Lots of Free Swing and metal theme
  additions. ``MetalIconFactory`` implementation.

* Anthony Green for ``MIDI`` framework, ``ALSA`` and ``DSSI``
  providers.

* Andrew Haley for ``Serialization`` and ``URLClassLoader`` fixes,
  gcj build speedups.

* Kim Ho for ``JFileChooser`` implementation.

* Andrew John Hughes for ``Locale`` and net fixes, URI RFC2986
  updates, ``Serialization`` fixes, ``Properties`` XML support and
  generic branch work, VMIntegration guide update.

* Bastiaan Huisman for ``TimeZone`` bug fixing.

* Andreas Jaeger for mprec updates.

* Paul Jenner for better :option:`-Werror` support.

* Ito Kazumitsu for ``NetworkInterface`` implementation and updates.

* Roman Kennke for ``BoxLayout``, ``GrayFilter`` and
  ``SplitPane``, plus bug fixes all over. Lots of Free Swing work
  including styled text.

* Simon Kitching for ``String`` cleanups and optimization suggestions.

* Michael Koch for configuration fixes, ``Locale`` updates, bug and
  build fixes.

* Guilhem Lavaux for configuration, thread and channel fixes and Kaffe
  integration. JCL native ``Pointer`` updates. Logger bug fixes.

* David Lichteblau for JCL support library global/local reference
  cleanups.

* Aaron Luchko for JDWP updates and documentation fixes.

* Ziga Mahkovec for ``Graphics2D`` upgraded to Cairo 0.5 and new regex
  features.

* Sven de Marothy for BMP imageio support, CSS and ``TextLayout``
  fixes. ``GtkImage`` rewrite, 2D, awt, free swing and date/time fixes and
  implementing the Qt4 peers.

* Casey Marshall for crypto algorithm fixes, ``FileChannel`` lock,
  ``SystemLogger`` and ``FileHandler`` rotate implementations, NIO
  ``FileChannel.map`` support, security and policy updates.

* Bryce McKinlay for RMI work.

* Audrius Meskauskas for lots of Free Corba, RMI and HTML work plus
  testing and documenting.

* Kalle Olavi Niemitalo for build fixes.

* Rainer Orth for build fixes.

* Andrew Overholt for ``File`` locking fixes.

* Ingo Proetel for ``Image``, ``Logger`` and ``URLClassLoader``
  updates.

* Olga Rodimina for ``MenuSelectionManager`` implementation.

* Jan Roehrich for ``BasicTreeUI`` and ``JTree`` fixes.

* Julian Scheid for documentation updates and gjdoc support.

* Christian Schlichtherle for zip fixes and cleanups.

* Robert Schuster for documentation updates and beans fixes,
  ``TreeNode`` enumerations and ``ActionCommand`` and various
  fixes, XML and URL, AWT and Free Swing bug fixes.

* Keith Seitz for lots of JDWP work.

* Christian Thalinger for 64-bit cleanups, Configuration and VM
  interface fixes and ``CACAO`` integration, ``fdlibm`` updates.

* Gael Thomas for ``VMClassLoader`` boot packages support suggestions.

* Andreas Tobler for Darwin and Solaris testing and fixing, ``Qt4``
  support for Darwin/OS X, ``Graphics2D`` support, ``gtk+``
  updates.

* Dalibor Topic for better ``DEBUG`` support, build cleanups and
  Kaffe integration. ``Qt4`` build infrastructure, ``SHA1PRNG``
  and ``GdkPixbugDecoder`` updates.

* Tom Tromey for Eclipse integration, generics work, lots of bug fixes
  and gcj integration including coordinating The Big Merge.

* Mark Wielaard for bug fixes, packaging and release management,
  ``Clipboard`` implementation, system call interrupts and network
  timeouts and ``GdkPixpufDecoder`` fixes.

In addition to the above, all of which also contributed time and energy in
testing GCC, we would like to thank the following for their contributions
to testing:

* Michael Abd-El-Malek

* Thomas Arend

* Bonzo Armstrong

* Steven Ashe

* Chris Baldwin

* David Billinghurst

* Jim Blandy

* Stephane Bortzmeyer

* Horst von Brand

* Frank Braun

* Rodney Brown

* Sidney Cadot

* Bradford Castalia

* Robert Clark

* Jonathan Corbet

* Ralph Doncaster

* Richard Emberson

* Levente Farkas

* Graham Fawcett

* Mark Fernyhough

* Robert A. French

* Jörgen Freyh

* Mark K. Gardner

* Charles-Antoine Gauthier

* Yung Shing Gene

* David Gilbert

* Simon Gornall

* Fred Gray

* John Griffin

* Patrik Hagglund

* Phil Hargett

* Amancio Hasty

* Takafumi Hayashi

* Bryan W. Headley

* Kevin B. Hendricks

* Joep Jansen

* Christian Joensson

* Michel Kern

* David Kidd

* Tobias Kuipers

* Anand Krishnaswamy

* A.O.V. Le Blanc

* llewelly

* Damon Love

* Brad Lucier

* Matthias Klose

* Martin Knoblauch

* Rick Lutowski

* Jesse Macnish

* Stefan Morrell

* Anon A. Mous

* Matthias Mueller

* Pekka Nikander

* Rick Niles

* Jon Olson

* Magnus Persson

* Chris Pollard

* Richard Polton

* Derk Reefman

* David Rees

* Paul Reilly

* Tom Reilly

* Torsten Rueger

* Danny Sadinoff

* Marc Schifer

* Erik Schnetter

* Wayne K. Schroll

* David Schuler

* Vin Shelton

* Tim Souder

* Adam Sulmicki

* Bill Thorson

* George Talbot

* Pedro A. M. Vazquez

* Gregory Warnes

* Ian Watson

* David E. Young

* And many others

And finally we'd like to thank everyone who uses the compiler, provides
feedback and generally reminds us why we're doing this work in the first
place.
