/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                 M K D I R                                *
 *                                                                          *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *            Copyright (C) 2002, Free Software Foundation, Inc.            *
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

/*  This file provides a portable binding to the mkdir() function           */

#ifdef __vxworks
#include "vxWorks.h"
#endif /* __vxworks */

#include <sys/types.h>
#include <sys/stat.h>

int
__gnat_mkdir (dir_name)
     char *dir_name;
{
#if defined (_WIN32) || defined (__vxworks)
  return mkdir (dir_name);
#else
  return mkdir (dir_name, S_IRWXU | S_IRWXG | S_IRWXO);
#endif
}
