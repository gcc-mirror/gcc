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

/* The following code corresponds to the Get_n_Bit_Field functions (for
   various n) in package Atree.  The low-level getters in sinfo.h call
   these even-lower-level getters.  */

extern Field_Offset *Node_Offsets_Ptr;
extern any_slot *Slots_Ptr;

INLINE Union_Id Get_1_Bit_Field (Node_Id N, Field_Offset Offset);
INLINE Union_Id Get_2_Bit_Field (Node_Id N, Field_Offset Offset);
INLINE Union_Id Get_4_Bit_Field (Node_Id N, Field_Offset Offset);
INLINE Union_Id Get_8_Bit_Field (Node_Id N, Field_Offset Offset);
INLINE Union_Id Get_32_Bit_Field (Node_Id N, Field_Offset Offset);
INLINE Union_Id Get_32_Bit_Field_With_Default (Node_Id N, Field_Offset Offset,
					       Union_Id Default_Value);

INLINE Union_Id
Get_1_Bit_Field (Node_Id N, Field_Offset Offset)
{
  const Field_Offset L = 32;

  slot_1_bit slot = (Slots_Ptr + (Node_Offsets_Ptr[N] + Offset / L))->slot_1;

  switch (Offset % L)
    {
    case 0: return slot.f0;
    case 1: return slot.f1;
    case 2: return slot.f2;
    case 3: return slot.f3;
    case 4: return slot.f4;
    case 5: return slot.f5;
    case 6: return slot.f6;
    case 7: return slot.f7;
    case 8: return slot.f8;
    case 9: return slot.f9;
    case 10: return slot.f10;
    case 11: return slot.f11;
    case 12: return slot.f12;
    case 13: return slot.f13;
    case 14: return slot.f14;
    case 15: return slot.f15;
    case 16: return slot.f16;
    case 17: return slot.f17;
    case 18: return slot.f18;
    case 19: return slot.f19;
    case 20: return slot.f20;
    case 21: return slot.f21;
    case 22: return slot.f22;
    case 23: return slot.f23;
    case 24: return slot.f24;
    case 25: return slot.f25;
    case 26: return slot.f26;
    case 27: return slot.f27;
    case 28: return slot.f28;
    case 29: return slot.f29;
    case 30: return slot.f30;
    case 31: return slot.f31;
    default: gcc_unreachable ();
    }
}

INLINE Union_Id
Get_2_Bit_Field (Node_Id N, Field_Offset Offset)
{
  const Field_Offset L = 16;

  slot_2_bit slot = (Slots_Ptr + (Node_Offsets_Ptr[N] + Offset / L))->slot_2;

  switch (Offset % L)
    {
    case 0: return slot.f0;
    case 1: return slot.f1;
    case 2: return slot.f2;
    case 3: return slot.f3;
    case 4: return slot.f4;
    case 5: return slot.f5;
    case 6: return slot.f6;
    case 7: return slot.f7;
    case 8: return slot.f8;
    case 9: return slot.f9;
    case 10: return slot.f10;
    case 11: return slot.f11;
    case 12: return slot.f12;
    case 13: return slot.f13;
    case 14: return slot.f14;
    case 15: return slot.f15;
    default: gcc_unreachable ();
    }
}

INLINE Union_Id
Get_4_Bit_Field (Node_Id N, Field_Offset Offset)
{
  const Field_Offset L = 8;

  slot_4_bit slot = (Slots_Ptr + (Node_Offsets_Ptr[N] + Offset / L))->slot_4;

  switch (Offset % L)
    {
    case 0: return slot.f0;
    case 1: return slot.f1;
    case 2: return slot.f2;
    case 3: return slot.f3;
    case 4: return slot.f4;
    case 5: return slot.f5;
    case 6: return slot.f6;
    case 7: return slot.f7;
    default: gcc_unreachable ();
    }
}

INLINE Union_Id
Get_8_Bit_Field (Node_Id N, Field_Offset Offset)
{
  const Field_Offset L = 4;

  slot_8_bit slot = (Slots_Ptr + (Node_Offsets_Ptr[N] + Offset / L))->slot_8;

  switch (Offset % L)
    {
    case 0: return slot.f0;
    case 1: return slot.f1;
    case 2: return slot.f2;
    case 3: return slot.f3;
    default: gcc_unreachable ();
    }
}

INLINE Union_Id
Get_32_Bit_Field (Node_Id N, Field_Offset Offset)
{
  const Field_Offset L = 1;

  slot_32_bit slot = (Slots_Ptr + (Node_Offsets_Ptr[N] + Offset / L))->slot_32;

  return slot;
}

INLINE Union_Id
Get_32_Bit_Field_With_Default (Node_Id N, Field_Offset Offset,
			       Union_Id Default_Value)
{
  const Field_Offset L = 1;

  slot_32_bit slot = (Slots_Ptr + (Node_Offsets_Ptr[N] + Offset / L))->slot_32;

  return slot == Empty ? Default_Value : slot;
}

#ifdef __cplusplus
}
#endif
