/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                            L A N G - S P E C S                           *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *           Copyright (C) 1992-2008, Free Software Foundation, Inc.        *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have received a copy of the GNU General   *
 * Public License along with GCC; see the file COPYING3.  If not see        *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/* This is the contribution to the `default_compilers' array in gcc.c for
   GNAT.  */

  {".ads", "@ada", 0, 0, 0},
  {".adb", "@ada", 0, 0, 0},
  {"@ada",
   "\
 %{pg:%{fomit-frame-pointer:%e-pg and -fomit-frame-pointer are incompatible}}\
 %{!S:%{!c:%e-c or -S required for Ada}}\
 gnat1 %{I*} %{k8:-gnatk8} %{Wall:-gnatwa} %{w:-gnatws} %{!Q:-quiet}\
    %{nostdinc*} %{nostdlib*}\
    -dumpbase %{.adb:%b.adb}%{.ads:%b.ads}%{!.adb:%{!.ads:%b.ada}}\
    %{O*} %{W*} %{w} %{p} %{pg:-p} %{a} %{d*} %{f*}\
    %{coverage:-fprofile-arcs -ftest-coverage} "
#if CONFIG_DUAL_EXCEPTIONS
   "%{fRTS=sjlj:-fsjlj} "
#endif
   "%{gnatea:-gnatez} %{g*&m*} "
#if defined(TARGET_VXWORKS_RTP)
   "%{fRTS=rtp:-mrtp} "
#endif
   "%1 %{!S:%{o*:%w%*-gnatO}} \
    %i %{S:%W{o*}%{!o*:-o %b.s}} \
    %{gnatc*|gnats*: -o %j} %{-param*} \
    %{!gnatc*:%{!gnats*:%(invoke_as)}}", 0, 0, 0},
