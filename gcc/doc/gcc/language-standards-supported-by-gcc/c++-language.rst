..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

C++ Language
************

GCC supports the original ISO C++ standard published in 1998,
and the 2011, 2014, 2017 and mostly 2020 revisions.

The original ISO C++ standard was published as the ISO standard (ISO/IEC
14882:1998) and amended by a Technical Corrigenda published in 2003
(ISO/IEC 14882:2003). These standards are referred to as C++98 and
C++03, respectively. GCC implements the majority of C++98 (``export``
is a notable exception) and most of the changes in C++03.  To select
this standard in GCC, use one of the options :option:`-ansi`,
:option:`-std=c++98`, or :option:`-std=c++03` ; to obtain all the diagnostics
required by the standard, you should also specify :option:`-pedantic` (or
:option:`-pedantic-errors` if you want them to be errors rather than
warnings).

A revised ISO C++ standard was published in 2011 as ISO/IEC
14882:2011, and is referred to as C++11; before its publication it was
commonly referred to as C++0x.  C++11 contains several changes to the
C++ language, all of which have been implemented in GCC. For details
see https://gcc.gnu.org/projects/cxx-status.html#cxx11.
To select this standard in GCC, use the option :option:`-std=c++11`.

Another revised ISO C++ standard was published in 2014 as ISO/IEC
14882:2014, and is referred to as C++14; before its publication it was
sometimes referred to as C++1y.  C++14 contains several further
changes to the C++ language, all of which have been implemented in GCC.
For details see https://gcc.gnu.org/projects/cxx-status.html#cxx14.
To select this standard in GCC, use the option :option:`-std=c++14`.

The C++ language was further revised in 2017 and ISO/IEC 14882:2017 was
published.  This is referred to as C++17, and before publication was
often referred to as C++1z.  GCC supports all the changes in that
specification.  For further details see
https://gcc.gnu.org/projects/cxx-status.html#cxx17.  Use the option
:option:`-std=c++17` to select this variant of C++.

Another revised ISO C++ standard was published in 2020 as ISO/IEC
14882:2020, and is referred to as C++20; before its publication it was
sometimes referred to as C++2a.  GCC supports most of the changes in the
new specification.  For further details see
https://gcc.gnu.org/projects/cxx-status.html#cxx20.
To select this standard in GCC, use the option :option:`-std=c++20`.

More information about the C++ standards is available on the ISO C++
committee's web site at http://www.open-std.org/jtc1/sc22/wg21/.

To obtain all the diagnostics required by any of the standard versions
described above you should specify :option:`-pedantic`
or :option:`-pedantic-errors`, otherwise GCC will allow some non-ISO C++
features as extensions. See :ref:`warning-options`.

By default, GCC also provides some additional extensions to the C++ language
that on rare occasions conflict with the C++ standard.  See :ref:`c++-dialect-options`.  Use of the
:option:`-std` options listed above disables these extensions where they
they conflict with the C++ standard version selected.  You may also
select an extended version of the C++ language explicitly with
:option:`-std=gnu++98` (for C++98 with GNU extensions), or
:option:`-std=gnu++11` (for C++11 with GNU extensions), or
:option:`-std=gnu++14` (for C++14 with GNU extensions), or
:option:`-std=gnu++17` (for C++17 with GNU extensions), or
:option:`-std=gnu++20` (for C++20 with GNU extensions).

The default, if
no C++ language dialect options are given, is :option:`-std=gnu++17`.
