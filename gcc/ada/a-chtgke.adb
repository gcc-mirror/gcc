------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                      A D A . C O N T A I N E R S .                       --
--             H A S H _ T A B L E S . G E N E R I C _ K E Y S              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2005 Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- This unit was originally developed by Matthew J Heaney.                  --
------------------------------------------------------------------------------

package body Ada.Containers.Hash_Tables.Generic_Keys is

   --------------------------
   -- Delete_Key_Sans_Free --
   --------------------------

   procedure Delete_Key_Sans_Free
     (HT   : in out Hash_Table_Type;
      Key  : Key_Type;
      X    : out Node_Access)
   is
      Indx : Hash_Type;
      Prev : Node_Access;

   begin
      if HT.Length = 0 then
         X := null;
         return;
      end if;

      Indx := Index (HT, Key);
      X := HT.Buckets (Indx);

      if X = null then
         return;
      end if;

      if Equivalent_Keys (Key, X) then
         if HT.Busy > 0 then
            raise Program_Error;
         end if;
         HT.Buckets (Indx) := Next (X);
         HT.Length := HT.Length - 1;
         return;
      end if;

      loop
         Prev := X;
         X := Next (Prev);

         if X = null then
            return;
         end if;

         if Equivalent_Keys (Key, X) then
            if HT.Busy > 0 then
               raise Program_Error;
            end if;
            Set_Next (Node => Prev, Next => Next (X));
            HT.Length := HT.Length - 1;
            return;
         end if;
      end loop;
   end Delete_Key_Sans_Free;

   ----------
   -- Find --
   ----------

   function Find
     (HT  : Hash_Table_Type;
      Key : Key_Type) return Node_Access is

      Indx : Hash_Type;
      Node : Node_Access;

   begin
      if HT.Length = 0 then
         return null;
      end if;

      Indx := Index (HT, Key);

      Node := HT.Buckets (Indx);
      while Node /= null loop
         if Equivalent_Keys (Key, Node) then
            return Node;
         end if;
         Node := Next (Node);
      end loop;

      return null;
   end Find;

   --------------------------------
   -- Generic_Conditional_Insert --
   --------------------------------

   procedure Generic_Conditional_Insert
     (HT       : in out Hash_Table_Type;
      Key      : Key_Type;
      Node     : out Node_Access;
      Inserted : out Boolean)
   is
      Indx : constant Hash_Type := Index (HT, Key);
      B    : Node_Access renames HT.Buckets (Indx);

      subtype Length_Subtype is Count_Type range 0 .. Count_Type'Last - 1;

   begin
      if B = null then
         if HT.Busy > 0 then
            raise Program_Error;
         end if;

         declare
            Length : constant Length_Subtype := HT.Length;
         begin
            Node := New_Node (Next => null);
            Inserted := True;

            B := Node;
            HT.Length := Length + 1;
         end;

         return;
      end if;

      Node := B;
      loop
         if Equivalent_Keys (Key, Node) then
            Inserted := False;
            return;
         end if;

         Node := Next (Node);

         exit when Node = null;
      end loop;

      if HT.Busy > 0 then
         raise Program_Error;
      end if;

      declare
         Length : constant Length_Subtype := HT.Length;
      begin
         Node := New_Node (Next => B);
         Inserted := True;

         B := Node;
         HT.Length := Length + 1;
      end;
   end Generic_Conditional_Insert;

   -----------
   -- Index --
   -----------

   function Index
     (HT  : Hash_Table_Type;
      Key : Key_Type) return Hash_Type is
   begin
      return Hash (Key) mod HT.Buckets'Length;
   end Index;

end Ada.Containers.Hash_Tables.Generic_Keys;
