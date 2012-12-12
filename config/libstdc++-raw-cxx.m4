# This file is part of GCC.
#
# GCC is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free
# Software Foundation; either version 3, or (at your option) any later
# version.
#
# GCC is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with GCC; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.

# Define flags, LIBSTDCXX_RAW_CXX_CXXFLAGS and # LIBSTDCXX_RAW_CXX_LDFLAGS,
# for libstdc++-v3 header files to compile and link libraries in C++ with
# raw_cxx=true.
AC_DEFUN([GCC_LIBSTDCXX_RAW_CXX_FLAGS], [
  AC_REQUIRE([ACX_NONCANONICAL_TARGET])
  LIBSTDCXX_RAW_CXX_CXXFLAGS="\
    -I\$(top_builddir)/../libstdc++-v3/include \
    -I\$(top_builddir)/../libstdc++-v3/include/\$(target_noncanonical) \
    -I\$(top_srcdir)/../libstdc++-v3/libsupc++"
  LIBSTDCXX_RAW_CXX_LDFLAGS="\
    -I\$(top_builddir)/../libstdc++-v3/src/libstdc++.la"
  AC_SUBST(LIBSTDCXX_RAW_CXX_CXXFLAGS)
  AC_SUBST(LIBSTDCXX_RAW_CXX_LDFLAGS)
])
