------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                 G N A T . D Y N A M I C _ H T A B L E S                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2002-2006, AdaCore                     --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

package body GNAT.Dynamic_HTables is

   -------------------
   -- Static_HTable --
   -------------------

   package body Static_HTable is

      type Table_Type is array (Header_Num) of Elmt_Ptr;

      type Instance_Data is record
         Table            : Table_Type;
         Iterator_Index   : Header_Num;
         Iterator_Ptr     : Elmt_Ptr;
         Iterator_Started : Boolean := False;
      end record;

      function Get_Non_Null (T : Instance) return Elmt_Ptr;
      --  Returns Null_Ptr if Iterator_Started is False or if the Table is
      --  empty. Returns Iterator_Ptr if non null, or the next non null
      --  element in table if any.

      ---------
      -- Get --
      ---------

      function  Get (T : Instance; K : Key) return Elmt_Ptr is
         Elmt  : Elmt_Ptr;

      begin
         if T = null then
            return Null_Ptr;
         end if;

         Elmt := T.Table (Hash (K));

         loop
            if Elmt = Null_Ptr then
               return Null_Ptr;

            elsif Equal (Get_Key (Elmt), K) then
               return Elmt;

            else
               Elmt := Next (Elmt);
            end if;
         end loop;
      end Get;

      ---------------
      -- Get_First --
      ---------------

      function Get_First (T : Instance) return Elmt_Ptr is
      begin
         if T = null then
            return Null_Ptr;
         end if;

         T.Iterator_Started := True;
         T.Iterator_Index := T.Table'First;
         T.Iterator_Ptr := T.Table (T.Iterator_Index);
         return Get_Non_Null (T);
      end Get_First;

      --------------
      -- Get_Next --
      --------------

      function Get_Next (T : Instance) return Elmt_Ptr is
      begin
         if T = null or else not T.Iterator_Started then
            return Null_Ptr;
         end if;

         T.Iterator_Ptr := Next (T.Iterator_Ptr);
         return Get_Non_Null (T);
      end Get_Next;

      ------------------
      -- Get_Non_Null --
      ------------------

      function Get_Non_Null (T : Instance) return Elmt_Ptr is
      begin
         if T = null then
            return Null_Ptr;
         end if;

         while T.Iterator_Ptr = Null_Ptr  loop
            if T.Iterator_Index = T.Table'Last then
               T.Iterator_Started := False;
               return Null_Ptr;
            end if;

            T.Iterator_Index := T.Iterator_Index + 1;
            T.Iterator_Ptr   := T.Table (T.Iterator_Index);
         end loop;

         return T.Iterator_Ptr;
      end Get_Non_Null;

      ------------
      -- Remove --
      ------------

      procedure Remove  (T : Instance; K : Key) is
         Index     : constant Header_Num := Hash (K);
         Elmt      : Elmt_Ptr;
         Next_Elmt : Elmt_Ptr;

      begin
         if T = null then
            return;
         end if;

         Elmt := T.Table (Index);

         if Elmt = Null_Ptr then
            return;

         elsif Equal (Get_Key (Elmt), K) then
            T.Table (Index) := Next (Elmt);

         else
            loop
               Next_Elmt :=  Next (Elmt);

               if Next_Elmt = Null_Ptr then
                  return;

               elsif Equal (Get_Key (Next_Elmt), K) then
                  Set_Next (Elmt, Next (Next_Elmt));
                  return;

               else
                  Elmt := Next_Elmt;
               end if;
            end loop;
         end if;
      end Remove;

      -----------
      -- Reset --
      -----------

      procedure Reset (T : in out Instance) is
         procedure Free is
           new Ada.Unchecked_Deallocation (Instance_Data, Instance);

      begin
         if T = null then
            return;
         end if;

         for J in T.Table'Range loop
            T.Table (J) := Null_Ptr;
         end loop;

         Free (T);
      end Reset;

      ---------
      -- Set --
      ---------

      procedure Set (T : in out Instance; E : Elmt_Ptr) is
         Index : Header_Num;

      begin
         if T = null then
            T := new Instance_Data;
         end if;

         Index := Hash (Get_Key (E));
         Set_Next (E, T.Table (Index));
         T.Table (Index) := E;
      end Set;

   end Static_HTable;

   -------------------
   -- Simple_HTable --
   -------------------

   package body Simple_HTable is

      ---------
      -- Get --
      ---------

      function  Get (T : Instance; K : Key) return Element is
         Tmp : Elmt_Ptr;

      begin
         if T = Nil then
            return No_Element;
         end if;

         Tmp := Tab.Get (Tab.Instance (T), K);

         if Tmp = null then
            return No_Element;
         else
            return Tmp.E;
         end if;
      end Get;

      ---------------
      -- Get_First --
      ---------------

      function Get_First (T : Instance) return Element is
         Tmp : constant Elmt_Ptr := Tab.Get_First (Tab.Instance (T));

      begin
         if Tmp = null then
            return No_Element;
         else
            return Tmp.E;
         end if;
      end Get_First;

      -------------
      -- Get_Key --
      -------------

      function Get_Key (E : Elmt_Ptr) return Key is
      begin
         return E.K;
      end Get_Key;

      --------------
      -- Get_Next --
      --------------

      function Get_Next (T : Instance) return Element is
         Tmp : constant Elmt_Ptr := Tab.Get_Next (Tab.Instance (T));
      begin
         if Tmp = null then
            return No_Element;
         else
            return Tmp.E;
         end if;
      end Get_Next;

      ----------
      -- Next --
      ----------

      function Next (E : Elmt_Ptr) return Elmt_Ptr is
      begin
         return E.Next;
      end Next;

      ------------
      -- Remove --
      ------------

      procedure Remove  (T : Instance; K : Key) is
         Tmp : Elmt_Ptr;

      begin
         Tmp := Tab.Get (Tab.Instance (T), K);

         if Tmp /= null then
            Tab.Remove (Tab.Instance (T), K);
            Free (Tmp);
         end if;
      end Remove;

      -----------
      -- Reset --
      -----------

      procedure Reset (T : in out Instance) is
         E1, E2 : Elmt_Ptr;

      begin
         E1 := Tab.Get_First (Tab.Instance (T));
         while E1 /= null loop
            E2 := Tab.Get_Next (Tab.Instance (T));
            Free (E1);
            E1 := E2;
         end loop;

         Tab.Reset (Tab.Instance (T));
      end Reset;

      ---------
      -- Set --
      ---------

      procedure Set (T : in out Instance; K : Key; E : Element) is
         Tmp : constant Elmt_Ptr := Tab.Get (Tab.Instance (T), K);
      begin
         if Tmp = null then
            Tab.Set (Tab.Instance (T), new Element_Wrapper'(K, E, null));
         else
            Tmp.E := E;
         end if;
      end Set;

      --------------
      -- Set_Next --
      --------------

      procedure Set_Next (E : Elmt_Ptr; Next : Elmt_Ptr) is
      begin
         E.Next := Next;
      end Set_Next;

   end Simple_HTable;

end GNAT.Dynamic_HTables;
