/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                               E L I S T S                                *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *                            $Revision: 1.1 $
 *                                                                          *
 *          Copyright (C) 1992-2001 Free Software Foundation, Inc.          *
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
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/* This is the C header corresponding to the Ada package specification for
   Elists. It also contains the implementations of inlined functions from the
   package body for Elists.  It was generated manually from elists.ads and
   elists.adb and must be kept synchronized with changes in these files.

   Note that only routines for reading the tree are included, since the
   tree transformer is not supposed to modify the tree in any way. */

/*  The following are the structures used to hold element lists */

struct Elist_Header
{
  Elmt_Id first;
  Elmt_Id last;
};

struct Elmt_Item
{
  Node_Id node;
  Int next;
};

/* The element list headers and element descriptors themselves are stored in
   two arrays. The pointers to these arrays are passed as a parameter to the
   tree transformer procedure and stored in the global variables Elists_Ptr
   and Elmts_Ptr after adjusting them by subtracting Elist_First_Entry and
   Elmt_First_Entry, so that Elist_Id and Elmt_Id values can be used as
   subscripts into these arrays */

extern struct Elist_Header *Elists_Ptr;
extern struct Elmt_Item *Elmts_Ptr;

/* Element List Access Functions:  */

static Node_Id Node		PARAMS ((Elmt_Id));
static Elmt_Id First_Elmt	PARAMS ((Elist_Id));
static Elmt_Id Last_Elmt	PARAMS ((Elist_Id));
static Elmt_Id Next_Elmt	PARAMS ((Elmt_Id));
static Boolean Is_Empty_Elmt_List PARAMS ((Elist_Id));

INLINE Node_Id
Node (Elmt)
     Elmt_Id Elmt;
{
  return Elmts_Ptr [Elmt].node;
}

INLINE Elmt_Id
First_Elmt (List)
     Elist_Id List;
{
  return Elists_Ptr [List].first;
}

INLINE Elmt_Id
Last_Elmt (List)
     Elist_Id List;
{
  return Elists_Ptr [List].last;
}

INLINE Elmt_Id
Next_Elmt (Node)
     Elmt_Id Node;
{
  Int N = Elmts_Ptr [Node].next;

  if (IN (N, Elist_Range))
    return No_Elmt;
  else
    return N;
}

INLINE Boolean
Is_Empty_Elmt_List (Id)
     Elist_Id Id;
{
  return Elists_Ptr [Id].first == No_Elmt;
}
