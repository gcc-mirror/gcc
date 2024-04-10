/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                R A I S E                                 *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *          Copyright (C) 1992-2024, Free Software Foundation, Inc.         *
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

#ifdef __cplusplus
extern "C" {
#endif

/* C counterparts of what System.Standard_Library defines.  */

struct Exception_Data
{
  char Not_Handled_By_Others;
  char Lang;
  int Name_Length;
  __UINTPTR_TYPE__ Full_Name;
  void *HTable_Ptr;
  __UINTPTR_TYPE__ Foreign_Data;
  void (*Raise_Hook)(void);
};

typedef struct Exception_Data *Exception_Id;

struct Exception_Occurrence;

extern void __gnat_unhandled_terminate	(void);
extern void *__gnat_malloc		(__SIZE_TYPE__);
extern void __gnat_free			(void *);
extern void *__gnat_realloc		(void *, __SIZE_TYPE__);
extern void __gnat_finalize		(void);
extern void __gnat_set_globals		(void);
extern void __gnat_initialize		(void *);
extern void __gnat_init_float		(void);
extern void __gnat_install_handler	(void);
extern void __gnat_install_SEH_handler  (void *);
extern void __gnat_adjust_context_for_raise (int, void *);

extern int gnat_exit_status;

#ifdef __cplusplus
}
#endif
