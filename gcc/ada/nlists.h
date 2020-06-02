/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                               N L I S T S                                *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *            Copyright (C) 1992-2020, Free Software Foundation, Inc.       *
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

/* This is the C header that corresponds to the Ada package specification for
   Nlists.  It also contains the implementation of inlined functions from the
   package body for Nlists.  It was created manually from nlists.ads and
   nlists.adb and must be kept synchronized with changes in these files.

   Note that only routines for reading the tree are included, since the
   tree transformer is not supposed to modify the tree in any way.  */

#ifdef __cplusplus
extern "C" {
#endif

/*  The following is the structure used for the list headers table */

struct List_Header
{
  Node_Id first;
  Node_Id last;
  Node_Id parent;
};

/* The list headers are stored in an array.  The pointer to this array is
   passed as a parameter to gigi and stored in the global variable
   List_Headers_Ptr.  */

extern struct List_Header *List_Headers_Ptr;

/* The previous and next links for lists are held in two arrays, Next_Node and
   Prev_Node.  The pointers to these arrays are passed as parameters to gigi
   and stored in the global variables Prev_Node_Ptr and Next_Node_Ptr.  */

extern Node_Id *Next_Node_Ptr;
extern Node_Id *Prev_Node_Ptr;

/* Node List Access Functions */

static Node_Id First (List_Id);

INLINE Node_Id
First (List_Id List)
{
  return List_Headers_Ptr[List - First_List_Id].first;
}

#define First_Non_Pragma nlists__first_non_pragma
extern Node_Id First_Non_Pragma (Node_Id);

static Node_Id Last (List_Id);

INLINE Node_Id
Last (List_Id List)
{
  return List_Headers_Ptr[List - First_List_Id].last;
}

#define First_Non_Pragma nlists__first_non_pragma
extern Node_Id First_Non_Pragma (List_Id);

static Node_Id Next (Node_Id);

INLINE Node_Id
Next (Node_Id Node)
{
  return Next_Node_Ptr[Node - First_Node_Id];
}

#define Next_Non_Pragma nlists__next_non_pragma
extern Node_Id Next_Non_Pragma (List_Id);

static Node_Id Prev (Node_Id);

INLINE Node_Id
Prev (Node_Id Node)
{
  return Prev_Node_Ptr[Node - First_Node_Id];
}


#define Prev_Non_Pragma nlists__prev_non_pragma
extern Node_Id Prev_Non_Pragma		(Node_Id);

static Boolean Is_Empty_List		(List_Id);
static Boolean Is_Non_Empty_List	(List_Id);
static Boolean Is_List_Member		(Node_Id);
static List_Id List_Containing		(Node_Id);

INLINE Boolean
Is_Empty_List (List_Id Id)
{
  return (First (Id) == Empty);
}

INLINE Boolean
Is_Non_Empty_List (List_Id Id)
{
  return (Present (Id) && First (Id) != Empty);
}

INLINE Boolean
Is_List_Member (Node_Id Node)
{
  return Nodes_Ptr[Node - First_Node_Id].U.K.in_list;
}

INLINE List_Id
List_Containing (Node_Id Node)
{
  return Nodes_Ptr[Node - First_Node_Id].V.NX.link;
}

#ifdef __cplusplus
}
#endif
