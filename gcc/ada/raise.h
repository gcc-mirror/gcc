/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                R A I S E                                 *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *          Copyright (C) 1992-2006, Free Software Foundation, Inc.         *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 2,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT;  see file COPYING.  If not, write *
 * to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, *
 * Boston, MA 02110-1301, USA.                                              *
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


/* C counterparts of what System.Standard_Library defines.  */

typedef unsigned Exception_Code;

struct Exception_Data
{
  char Not_Handled_By_Others;
  char Lang;
  int Name_Length;
  char *Full_Name, *Htable_Ptr;
  Exception_Code Import_Code;
  void (*Raise_Hook)(void);
};

typedef struct Exception_Data *Exception_Id;

struct Exception_Occurrence
{
  int Max_Length;
  Exception_Id Id;
  int Msg_Length;
  char Msg[0];
};

typedef struct Exception_Occurrence *Exception_Occurrence_Access;

extern void _gnat_builtin_longjmp	(void *, int);
extern void __gnat_unhandled_terminate	(void);
extern void *__gnat_malloc		(__SIZE_TYPE__);
extern void __gnat_free			(void *);
extern void *__gnat_realloc		(void *, __SIZE_TYPE__);
extern void __gnat_finalize		(void);
extern void set_gnat_exit_status	(int);
extern void __gnat_set_globals		(void);
extern void __gnat_initialize		(void *);
extern void __gnat_init_float		(void);
extern void __gnat_install_handler	(void);
extern void __gnat_install_SEH_handler  (void *);
extern void __gnat_adjust_context_for_raise (int, void *);

extern int gnat_exit_status;
