------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                      A D A . C O N T A I N E R S .                       --
--             H A S H _ T A B L E S . G E N E R I C _ K E Y S              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2006, Free Software Foundation, Inc.         --
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

   begin
      if B = null then
         if HT.Busy > 0 then
            raise Program_Error;
         end if;

         if HT.Length = Count_Type'Last then
            raise Constraint_Error;
         end if;

         Node := New_Node (Next => null);
         Inserted := True;

         B := Node;
         HT.Length := HT.Length + 1;

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

      if HT.Length = Count_Type'Last then
         raise Constraint_Error;
      end if;

      Node := New_Node (Next => B);
      Inserted := True;

      B := Node;
      HT.Length := HT.Length + 1;
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

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Generic_Replace_Element
     (HT   : in out Hash_Table_Type;
      Node : Node_Access;
      Key  : Key_Type)
   is
   begin
      pragma Assert (HT.Length > 0);

      if Equivalent_Keys (Key, Node) then
         pragma Assert (Hash (Key) = Hash (Node));

         if HT.Lock > 0 then
            raise Program_Error with
              "attempt to tamper with cursors (container is locked)";
         end if;

         Assign (Node, Key);
         return;
      end if;

      declare
         J : Hash_Type;
         K : constant Hash_Type := Index (HT, Key);
         B : Node_Access renames HT.Buckets (K);
         N : Node_Access := B;
         M : Node_Access;

      begin
         while N /= null loop
            if Equivalent_Keys (Key, N) then
               raise Program_Error with
                 "attempt to replace existing element";
            end if;

            N := Next (N);
         end loop;

         J := Hash (Node);

         if J = K then
            if HT.Lock > 0 then
               raise Program_Error with
                 "attempt to tamper with cursors (container is locked)";
            end if;

            Assign (Node, Key);
            return;
         end if;

         if HT.Busy > 0 then
            raise Program_Error with
              "attempt to tamper with elements (container is busy)";
         end if;

         Assign (Node, Key);

         N := HT.Buckets (J);
         pragma Assert (N /= null);

         if N = Node then
            HT.Buckets (J) := Next (Node);

         else
            pragma Assert (HT.Length > 1);

            loop
               M := Next (N);
               pragma Assert (M /= null);

               if M = Node then
                  Set_Next (Node => N, Next => Next (Node));
                  exit;
               end if;

               N := M;
            end loop;
         end if;

         Set_Next (Node => Node, Next => B);
         B := Node;
      end;
   end Generic_Replace_Element;

end Ada.Containers.Hash_Tables.Generic_Keys;
