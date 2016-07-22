/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                 M K D I R                                *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *             Copyright (C) 2002-2016, Free Software Foundation, Inc.      *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.                                     *
 *                                                                          *
 * As a special exception under Section 7 of GPL version 3, you are granted *
 * additional permissions described in the GCC Runtime Library Exception,   *
 * version 3.1, as published by the Free Software Foundation.               *
 *                                                                          *
 * You should have received a copy of the GNU General Public License and    *
 * a copy of the GCC Runtime Library Exception along with this program;     *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

#ifdef __vxworks
#include "vxWorks.h"
#include <version.h>
#endif /* __vxworks */

#ifdef IN_RTS
#include "tconfig.h"
#include "tsystem.h"
#include <sys/stat.h>
#else
#include "config.h"
#include "system.h"
#endif

#ifdef __MINGW32__
#include "mingw32.h"
#include <windows.h>
#ifdef MAXPATHLEN
#define GNAT_MAX_PATH_LEN MAXPATHLEN
#else
#define GNAT_MAX_PATH_LEN 256
#endif
#endif

#include "adaint.h"

/*  This function provides a portable binding to the mkdir function.  */

int
__gnat_mkdir (char *dir_name, int encoding ATTRIBUTE_UNUSED)
{
#if defined (__vxworks)

  /* Pretend that the system mkdir is posix compliant even though it
     sometimes is not, not expecting the second argument in some
     configurations (e.g. vxworks 653 2.2, difference from 2.5). The
     second actual argument will just be ignored in this case.  */

  typedef int posix_mkdir (const char * name, mode_t mode);

  posix_mkdir * vxmkdir = (posix_mkdir *)&mkdir;
  return vxmkdir (dir_name, S_IRWXU | S_IRWXG | S_IRWXO);

#elif defined (__MINGW32__)
  TCHAR wname [GNAT_MAX_PATH_LEN + 2];

  if (encoding == Encoding_Unspecified)
    S2WSC (wname, dir_name, GNAT_MAX_PATH_LEN);
  else if (encoding == Encoding_UTF8)
    S2WSU (wname, dir_name, GNAT_MAX_PATH_LEN);
  else
    S2WS (wname, dir_name, GNAT_MAX_PATH_LEN);

  return _tmkdir (wname);
#else
  return mkdir (dir_name, S_IRWXU | S_IRWXG | S_IRWXO);
#endif
}
