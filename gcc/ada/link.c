/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                 L I N K                                  *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *          Copyright (C) 1992-2004, Free Software Foundation, Inc.         *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 2,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT;  see file COPYING.  If not, write *
 * to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, *
 * MA 02111-1307, USA.                                                      *
 *                                                                          *
 * As a  special  exception,  if you  link  this file  with other  files to *
 * produce an executable,  this file does not by itself cause the resulting *
 * executable to be covered by the GNU General Public License. This except- *
 * ion does not  however invalidate  any other reasons  why the  executable *
 * file might be covered by the  GNU Public License.                        *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/*  This file contains parameterizations used by gnatlink.adb in handling   */
/*  very long linker lines in systems where there are limitations on the    */
/*  argument length when the command line is used to pass items to the      */
/*  linker                                                                  */

#include <string.h>

/*  objlist_file_supported is set to 1 when the system linker allows        */
/*  response file, that is a file that contains the list of object files.   */
/*  This is useful on systems where the command line length is limited,     */
/*  meaning that putting all the object files on the command line can       */
/*  result in an unacceptable limit on the number of files.                 */

/*  object_file_option denotes the system dependent linker option which     */
/*  allows object file names to be placed in a file and then passed to      */
/*  the linker. object_file_option must be set if objlist_file_supported    */
/*  is set to 1.                                                            */

/*  link_max is a conservative system specific threshold (in bytes) of the  */
/*  argument length passed to the linker which will trigger a file being    */
/*  used instead of the command line directly. If the argument length is    */
/*  greater than this threshhold, then an objlist_file will be generated    */
/*  and object_file_option and objlist_file_supported must be set. If       */
/*  objlist_file_supported is set to 0 (unsupported), then link_max is      */
/*  set to 2**31-1 so that the limit will never be exceeded.                */

/*  run_path_option is the system dependent linker option which specifies   */
/*  the run time path to use when loading dynamic libraries. This should    */
/*  be set to the null string if the system does not support dynmamic       */
/*  loading of libraries.                                                   */

/*  shared_libgnat_default gives the system dependent link method that      */
/*  be used by default for linking libgnat (shared or static)               */

/*  using_gnu_linker is set to 1 when the GNU linker is used under this     */
/*  target.                                                                 */

/*  RESPONSE FILE & GNU LINKER                                              */
/*  --------------------------                                              */
/*  objlist_file_supported and using_gnu_link used together tell gnatlink   */
/*  to generate a GNU style response file. Note that object_file_option     */
/*  must be set to "" in this case, since no option is required for a       */
/*  response file to be passed to GNU ld. With a GNU linker we use the      */
/*  linker script to implement the response file feature. Any file passed   */
/*  in the GNU ld command line with an unknown extension is supposed to be  */
/*  a linker script. Each linker script augment the current configuration.  */
/*  The format of such response file is as follow :                         */
/*  INPUT (obj1.p obj2.o ...)                                               */

#define SHARED 'H'
#define STATIC 'T'

#if defined (__osf__)
const char *object_file_option = "-Wl,-input,";
const char *run_path_option = "-Wl,-rpath,";
int link_max = 10000;
unsigned char objlist_file_supported = 1;
char shared_libgnat_default = STATIC;
unsigned char using_gnu_linker = 0;
const char *object_library_extension = ".a";

#elif defined (sgi)
const char *object_file_option = "-Wl,-objectlist,";
const char *run_path_option = "-Wl,-rpath,";
int link_max = 5000;
unsigned char objlist_file_supported = 1;
char shared_libgnat_default = STATIC;
unsigned char using_gnu_linker = 0;
const char *object_library_extension = ".a";

#elif defined (__WIN32)
const char *object_file_option = "";
const char *run_path_option = "";
int link_max = 30000;
unsigned char objlist_file_supported = 1;
char shared_libgnat_default = STATIC;
unsigned char using_gnu_linker = 1;
const char *object_library_extension = ".a";

#elif defined (__INTERIX)
const char *object_file_option = "";
const char *run_path_option = "";
int link_max = 5000;
unsigned char objlist_file_supported = 1;
char shared_libgnat_default = STATIC;
unsigned char using_gnu_linker = 1;
const char *object_library_extension = ".a";

#elif defined (hpux)
const char *object_file_option = "-Wl,-c,";
const char *run_path_option = "-Wl,+b,";
int link_max = 5000;
unsigned char objlist_file_supported = 1;
char shared_libgnat_default = STATIC;
unsigned char using_gnu_linker = 0;
const char *object_library_extension = ".a";

#elif defined (_AIX)
const char *object_file_option = "-Wl,-f,";
const char *run_path_option = "";
int link_max = 15000;
const unsigned char objlist_file_supported = 1;
char shared_libgnat_default = STATIC;
unsigned char using_gnu_linker = 0;
const char *object_library_extension = ".a";

#elif defined (VMS)
const char *object_file_option = "";
const char *run_path_option = "";
char shared_libgnat_default = STATIC;
int link_max = 2147483647;
unsigned char objlist_file_supported = 0;
unsigned char using_gnu_linker = 0;
const char *object_library_extension = ".olb";

#elif defined (sun)
const char *object_file_option = "";
const char *run_path_option = "-R";
char shared_libgnat_default = STATIC;
int link_max = 2147483647;
unsigned char objlist_file_supported = 0;
unsigned char using_gnu_linker = 0;
const char *object_library_extension = ".a";

#elif defined (__FreeBSD__)
char *object_file_option = "";
char *run_path_option = "";
char shared_libgnat_default = STATIC;
int link_max = 2147483647;
unsigned char objlist_file_supported = 0;
unsigned char using_gnu_linker = 0;
char *object_library_extension = ".a";

#elif defined (linux)
const char *object_file_option = "";
const char *run_path_option = "-Wl,-rpath,";
char shared_libgnat_default = STATIC;
int link_max = 8192;
unsigned char objlist_file_supported = 1;
unsigned char using_gnu_linker = 1;
const char *object_library_extension = ".a";

#elif defined (__svr4__) && defined (i386)
const char *object_file_option = "";
const char *run_path_option = "";
char shared_libgnat_default = STATIC;
int link_max = 2147483647;
unsigned char objlist_file_supported = 0;
unsigned char using_gnu_linker = 0;
const char *object_library_extension = ".a";

#else

/*  These are the default settings for all other systems. No response file
    is supported, the shared library default is STATIC.  */
const char *run_path_option = "";
const char *object_file_option = "";
char shared_libgnat_default = STATIC;
int link_max = 2147483647;
unsigned char objlist_file_supported = 0;
unsigned char using_gnu_linker = 0;
const char *object_library_extension = ".a";
#endif
