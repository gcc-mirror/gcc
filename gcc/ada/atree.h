/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                A T R E E                                 *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *          Copyright (C) 1992-2021, Free Software Foundation, Inc.         *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT; see file COPYING3.  If not, go to *
 * http://www.gnu.org/licenses for a complete copy of the license.          *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/* This is the C header that corresponds to the Ada package specification for
   Atree.  It also contains the implementation of inlined functions from the
   package body for Atree.  It was created manually from atree.ads and
   atree.adb and must be kept synchronized with changes in these files.

   Note that only routines for reading the tree are included, since the tree
   transformer is not supposed to modify the tree in any way.  */

#ifdef __cplusplus
extern "C" {
#endif

#define Parent atree__parent
extern Node_Id Parent (Node_Id);

#define Original_Node atree__original_node
extern Node_Id Original_Node (Node_Id);

/* Type used for union of Node_Id, List_Id, Elist_Id.  */
typedef Int Tree_Id;

/* These two functions can only be used for Node_Id and List_Id values and
   they work in the C version because Empty = No_List = 0.  */

INLINE Boolean No (Tree_Id);
INLINE Boolean Present (Tree_Id);

INLINE Boolean
No (Tree_Id N)
{
  return N == Empty;
}

INLINE Boolean
Present (Tree_Id N)
{
  return !No (N);
}

#define Current_Error_Node atree__current_error_node
extern Node_Id Current_Error_Node;

#ifdef __cplusplus
}
#endif
