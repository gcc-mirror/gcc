------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               S Y S T E M . S E C O N D A R Y _ S T A C K                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2004 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This is the HI-E version of this package.

with Unchecked_Conversion;

package body System.Secondary_Stack is

   use type SSE.Storage_Offset;

   type Memory is array (Mark_Id range <>) of SSE.Storage_Element;

   type Stack_Id is record
      Top  : Mark_Id;
      Last : Mark_Id;
      Mem  : Memory (1 .. Mark_Id'Last);
   end record;
   pragma Suppress_Initialization (Stack_Id);

   type Stack_Ptr is access Stack_Id;

   function From_Addr is new Unchecked_Conversion (Address, Stack_Ptr);

   function Get_Sec_Stack return Stack_Ptr;
   pragma Import (C, Get_Sec_Stack, "__gnat_get_secondary_stack");
   --  Return the address of the secondary stack.
   --  In a multi-threaded environment, Sec_Stack should be a thread-local
   --  variable.

   --  Possible implementation of Get_Sec_Stack in a single-threaded
   --  environment:
   --
   --     Chunk : aliased Memory (1 .. Default_Secondary_Stack_Size);
   --     for Chunk'Alignment use Standard'Maximum_Alignment;
   --     --  The secondary stack.
   --
   --     function Get_Sec_Stack return Stack_Ptr is
   --     begin
   --        return From_Addr (Chunk'Address);
   --     end Get_Sec_Stack;
   --
   --  begin
   --     SS_Init (Chunk'Address, Default_Secondary_Stack_Size);
   --  end System.Secondary_Stack;

   -----------------
   -- SS_Allocate --
   -----------------

   procedure SS_Allocate
     (Address      : out System.Address;
      Storage_Size : SSE.Storage_Count)
   is
      Max_Align    : constant Mark_Id := Mark_Id (Standard'Maximum_Alignment);
      Max_Size     : constant Mark_Id :=
                       ((Mark_Id (Storage_Size) + Max_Align - 1) / Max_Align)
                         * Max_Align;
      Sec_Stack    : constant Stack_Ptr := Get_Sec_Stack;

   begin
      if Sec_Stack.Top + Max_Size > Sec_Stack.Last then
         raise Storage_Error;
      end if;

      Address := Sec_Stack.Mem (Sec_Stack.Top)'Address;
      Sec_Stack.Top := Sec_Stack.Top + Max_Size;
   end SS_Allocate;

   -------------
   -- SS_Free --
   -------------

   procedure SS_Free (Stk : in out System.Address) is
   begin
      Stk := Null_Address;
   end SS_Free;

   -------------
   -- SS_Init --
   -------------

   procedure SS_Init
     (Stk  : System.Address;
      Size : Natural := Default_Secondary_Stack_Size)
   is
      Stack : Stack_Ptr := From_Addr (Stk);
   begin
      pragma Assert (Size >= 2 * Mark_Id'Max_Size_In_Storage_Elements);

      Stack.Top := Stack.Mem'First;
      Stack.Last := Mark_Id (Size) - 2 * Mark_Id'Max_Size_In_Storage_Elements;
   end SS_Init;

   -------------
   -- SS_Mark --
   -------------

   function SS_Mark return Mark_Id is
   begin
      return Get_Sec_Stack.Top;
   end SS_Mark;

   ----------------
   -- SS_Release --
   ----------------

   procedure SS_Release (M : Mark_Id) is
   begin
      Get_Sec_Stack.Top := M;
   end SS_Release;

end System.Secondary_Stack;
