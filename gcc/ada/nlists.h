/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                               N L I S T S                                *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *                            $Revision: 1.1 $
 *                                                                          *
 *          Copyright (C) 1992-2001, Free Software Foundation, Inc.         *
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
   Nlists. It also contains the implementations of inlined functions from the
   the package body for Nlists.  It was generated manually from nlists.ads and
   nlists.adb and must be kept synchronized with changes in these files.

   Note that only routines for reading the tree are included, since the
   tree transformer is not supposed to modify the tree in any way. */

/*  The following is the structure used for the list headers table */

struct List_Header
{
  Node_Id first;
  Node_Id last;
  Node_Id parent;
};

/* The list headers are stored in an array.  The pointer to this array is
   passed as a parameter to gigi and stored in the global variable
   List_Headers_Ptr after adjusting it by subtracting List_First_Entry,
   so that List_Id values can be used as subscripts.	*/

extern struct List_Header *List_Headers_Ptr;

/* The previous and next links for lists are held in two arrays, Next_Node
   and Prev_Node.  The pointers to these arrays are passed as parameters
   to gigi and stored in the global variables Prev_Node_Ptr and Next_Node_Ptr
   after adjusting them by subtracting First_Node_Id so that Node_Id values
   can be used as subscripts.  */

extern Node_Id *Next_Node_Ptr;
extern Node_Id *Prev_Node_Ptr;

/* Node List Access Functions */

static Node_Id First PARAMS ((List_Id));

INLINE Node_Id
First (List)
     List_Id List;
{
  return List_Headers_Ptr [List].first;
}

#define First_Non_Pragma nlists__first_non_pragma
extern Node_Id First_Non_Pragma PARAMS((Node_Id));

static Node_Id Last PARAMS ((List_Id));

INLINE Node_Id
Last (List)
     List_Id List;
{
  return List_Headers_Ptr [List].last;
}

#define First_Non_Pragma nlists__first_non_pragma
extern Node_Id First_Non_Pragma PARAMS((List_Id));

static Node_Id Next PARAMS ((Node_Id));

INLINE Node_Id
Next (Node)
     Node_Id Node;
{
  return Next_Node_Ptr [Node];
}

#define Next_Non_Pragma nlists__next_non_pragma
extern Node_Id Next_Non_Pragma PARAMS((List_Id));

static Node_Id Prev PARAMS ((Node_Id));

INLINE Node_Id
Prev (Node)
     Node_Id Node;
{
  return Prev_Node_Ptr [Node];
}


#define Prev_Non_Pragma nlists__prev_non_pragma
extern Node_Id Prev_Non_Pragma PARAMS((Node_Id));

static Boolean Is_Empty_List		PARAMS ((List_Id));
static Boolean Is_Non_Empty_List	PARAMS ((List_Id));
static Boolean Is_List_Member		PARAMS ((Node_Id));
static List_Id List_Containing		PARAMS ((Node_Id));

INLINE Boolean
Is_Empty_List (Id)
     List_Id Id;
{
  return (First (Id) == Empty);
}

INLINE Boolean
Is_Non_Empty_List (Id)
     List_Id Id;
{
  return (Present (Id) && First (Id) != Empty);
}

INLINE Boolean
Is_List_Member (Node)
     Node_Id Node;
{
  return Nodes_Ptr [Node].U.K.in_list;
}

INLINE List_Id
List_Containing (Node)
     Node_Id Node;
{
  return Nodes_Ptr [Node].V.NX.link;
}
