------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ D I S P                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2005 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains routines involved in tagged types and dynamic
--  dispatching expansion.

with Types; use Types;
package Exp_Disp is

   type DT_Access_Action is
      (CW_Membership,
       IW_Membership,
       DT_Entry_Size,
       DT_Prologue_Size,
       Get_Access_Level,
       Get_External_Tag,
       Get_Prim_Op_Address,
       Get_RC_Offset,
       Get_Remotely_Callable,
       Inherit_DT,
       Inherit_TSD,
       Register_Interface_Tag,
       Register_Tag,
       Set_Access_Level,
       Set_Expanded_Name,
       Set_External_Tag,
       Set_Prim_Op_Address,
       Set_RC_Offset,
       Set_Remotely_Callable,
       Set_TSD,
       TSD_Entry_Size,
       TSD_Prologue_Size);

   function Fill_DT_Entry
     (Loc          : Source_Ptr;
      Prim         : Entity_Id) return Node_Id;
   --  Generate the code necessary to fill the appropriate entry of the
   --  dispatch table of Prim's controlling type with Prim's address.

   function Fill_Secondary_DT_Entry
     (Loc          : Source_Ptr;
      Prim         : Entity_Id;
      Thunk_Id     : Entity_Id;
      Iface_DT_Ptr : Entity_Id) return Node_Id;
   --  (Ada 2005): Generate the code necessary to fill the appropriate entry of
   --  the secondary dispatch table of Prim's controlling type with Thunk_Id's
   --  address.

   procedure Make_Abstract_Interface_DT
     (AI_Tag          : Entity_Id;
      Acc_Disp_Tables : in out Elist_Id;
      Result          : out List_Id);
   --  Ada 2005 (AI-251): Expand the declarations for the secondary Dispatch
   --  Tables corresponding with an abstract interface. The reference to the
   --  dispatch table is appended at the end of Acc_Disp_Tables; it will be
   --  are later used to generate the corresponding initialization statement
   --  (see Exp_Ch3.Build_Init_Procedure).

   function Make_DT_Access_Action
     (Typ    : Entity_Id;
      Action : DT_Access_Action;
      Args   : List_Id) return Node_Id;
   --  Generate a call to one of the Dispatch Table Access Subprograms defined
   --  in Ada.Tags or in Interfaces.Cpp

   function Make_DT (Typ : Entity_Id) return List_Id;
   --  Expand the declarations for the Dispatch Table (or the Vtable in
   --  the case of type whose ancestor is a CPP_Class)

   procedure Set_All_DT_Position (Typ : Entity_Id);
   --  Set the DT_Position field for each primitive operation. In the CPP
   --  Class case check that no pragma CPP_Virtual is missing and that the
   --  DT_Position are coherent

   procedure Expand_Dispatching_Call (Call_Node : Node_Id);
   --  Expand the call to the operation through the dispatch table and perform
   --  the required tag checks when appropriate. For CPP types the call is
   --  done through the Vtable (tag checks are not relevant)

   procedure Expand_Interface_Actuals    (Call_Node : Node_Id);
   --  Ada 2005 (AI-251): Displace all the actuals corresponding to class-wide
   --  interfaces to reference the interface tag of the actual object

   procedure Expand_Interface_Conversion (N : Node_Id);
   --  Ada 2005 (AI-251): N is a type-conversion node. Reference the base of
   --  the object to give access to the interface tag associated with the
   --  secondary dispatch table

   function Expand_Interface_Thunk
     (N           : Node_Id;
      Thunk_Alias : Node_Id;
      Thunk_Id    : Entity_Id;
      Iface_Tag   : Entity_Id) return Node_Id;
   --  Ada 2005 (AI-251): When a tagged type implements abstract interfaces we
   --  generate additional subprograms (thunks) to have a layout compatible
   --  with the C++ ABI. The thunk modifies the value of the first actual of
   --  the call (that is, the pointer to the object) before transferring
   --  control to the target function.

   procedure Set_Default_Constructor (Typ : Entity_Id);
   --  Typ is a CPP_Class type. Create the Init procedure of that type to
   --  be the default constructor (i.e. the function returning this type,
   --  having a pragma CPP_Constructor and no parameter)

   function Get_Remotely_Callable (Obj : Node_Id) return Node_Id;
   --  Return an expression that holds True if the object can be transmitted
   --  onto another partition according to E.4 (18)

   procedure Write_DT (Typ : Entity_Id);
   pragma Export (Ada, Write_DT);
   --  Debugging procedure (to be called within gdb)

end Exp_Disp;
