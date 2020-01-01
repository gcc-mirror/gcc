------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                        S Y S T E M . H T A B L E                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 1995-2020, AdaCore                      --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

pragma Compiler_Unit_Warning;

with Ada.Unchecked_Deallocation;
with System.String_Hash;

package body System.HTable is

   -------------------
   -- Static_HTable --
   -------------------

   package body Static_HTable is

      Table : array (Header_Num) of Elmt_Ptr;

      Iterator_Index   : Header_Num;
      Iterator_Ptr     : Elmt_Ptr;
      Iterator_Started : Boolean := False;

      function Get_Non_Null return Elmt_Ptr;
      --  Returns Null_Ptr if Iterator_Started is false or the Table is empty.
      --  Returns Iterator_Ptr if non null, or the next non null element in
      --  table if any.

      ---------
      -- Get --
      ---------

      function Get (K : Key) return Elmt_Ptr is
         Elmt : Elmt_Ptr;

      begin
         Elmt := Table (Hash (K));
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

      function Get_First return Elmt_Ptr is
      begin
         Iterator_Started := True;
         Iterator_Index   := Table'First;
         Iterator_Ptr     := Table (Iterator_Index);
         return Get_Non_Null;
      end Get_First;

      --------------
      -- Get_Next --
      --------------

      function Get_Next return Elmt_Ptr is
      begin
         if not Iterator_Started then
            return Null_Ptr;
         else
            Iterator_Ptr := Next (Iterator_Ptr);
            return Get_Non_Null;
         end if;
      end Get_Next;

      ------------------
      -- Get_Non_Null --
      ------------------

      function Get_Non_Null return Elmt_Ptr is
      begin
         while Iterator_Ptr = Null_Ptr loop
            if Iterator_Index = Table'Last then
               Iterator_Started := False;
               return Null_Ptr;
            end if;

            Iterator_Index := Iterator_Index + 1;
            Iterator_Ptr   := Table (Iterator_Index);
         end loop;

         return Iterator_Ptr;
      end Get_Non_Null;

      -------------
      -- Present --
      -------------

      function Present (K : Key) return Boolean is
      begin
         return Get (K) /= Null_Ptr;
      end Present;

      ------------
      -- Remove --
      ------------

      procedure Remove  (K : Key) is
         Index     : constant Header_Num := Hash (K);
         Elmt      : Elmt_Ptr;
         Next_Elmt : Elmt_Ptr;

      begin
         Elmt := Table (Index);

         if Elmt = Null_Ptr then
            return;

         elsif Equal (Get_Key (Elmt), K) then
            Table (Index) := Next (Elmt);

         else
            loop
               Next_Elmt := Next (Elmt);

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

      procedure Reset is
      begin
         --  Use an aggregate for efficiency reasons

         Table := (others => Null_Ptr);
      end Reset;

      ---------
      -- Set --
      ---------

      procedure Set (E : Elmt_Ptr) is
         Index : Header_Num;
      begin
         Index := Hash (Get_Key (E));
         Set_Next (E, Table (Index));
         Table (Index) := E;
      end Set;

      ------------------------
      -- Set_If_Not_Present --
      ------------------------

      function Set_If_Not_Present (E : Elmt_Ptr) return Boolean is
         K : Key renames Get_Key (E);
         --  Note that it is important to use a renaming here rather than
         --  define a constant initialized by the call, because the latter
         --  construct runs into bootstrap problems with earlier versions
         --  of the GNAT compiler.

         Index : constant Header_Num := Hash (K);
         Elmt  : Elmt_Ptr;

      begin
         Elmt := Table (Index);
         loop
            if Elmt = Null_Ptr then
               Set_Next (E, Table (Index));
               Table (Index) := E;
               return True;

            elsif Equal (Get_Key (Elmt), K) then
               return False;

            else
               Elmt := Next (Elmt);
            end if;
         end loop;
      end Set_If_Not_Present;

   end Static_HTable;

   -------------------
   -- Simple_HTable --
   -------------------

   package body Simple_HTable is

      type Element_Wrapper;
      type Elmt_Ptr is access all Element_Wrapper;
      type Element_Wrapper is record
         K    : Key;
         E    : Element;
         Next : Elmt_Ptr;
      end record;

      procedure Free is new
        Ada.Unchecked_Deallocation (Element_Wrapper, Elmt_Ptr);

      procedure Set_Next (E : Elmt_Ptr; Next : Elmt_Ptr);
      function  Next     (E : Elmt_Ptr) return Elmt_Ptr;
      function  Get_Key  (E : Elmt_Ptr) return Key;

      package Tab is new Static_HTable (
        Header_Num => Header_Num,
        Element    => Element_Wrapper,
        Elmt_Ptr   => Elmt_Ptr,
        Null_Ptr   => null,
        Set_Next   => Set_Next,
        Next       => Next,
        Key        => Key,
        Get_Key    => Get_Key,
        Hash       => Hash,
        Equal      => Equal);

      ---------
      -- Get --
      ---------

      function Get (K : Key) return Element is
         Tmp : constant Elmt_Ptr := Tab.Get (K);
      begin
         if Tmp = null then
            return No_Element;
         else
            return Tmp.E;
         end if;
      end Get;

      ---------------
      -- Get_First --
      ---------------

      function Get_First return Element is
         Tmp : constant Elmt_Ptr := Tab.Get_First;
      begin
         if Tmp = null then
            return No_Element;
         else
            return Tmp.E;
         end if;
      end Get_First;

      procedure Get_First (K : in out Key; E : out Element) is
         Tmp : constant Elmt_Ptr := Tab.Get_First;
      begin
         if Tmp = null then
            E := No_Element;
         else
            K := Tmp.K;
            E := Tmp.E;
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

      function Get_Next return Element is
         Tmp : constant Elmt_Ptr := Tab.Get_Next;
      begin
         if Tmp = null then
            return No_Element;
         else
            return Tmp.E;
         end if;
      end Get_Next;

      procedure Get_Next (K : in out Key; E : out Element) is
         Tmp : constant Elmt_Ptr := Tab.Get_Next;
      begin
         if Tmp = null then
            E := No_Element;
         else
            K := Tmp.K;
            E := Tmp.E;
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

      procedure Remove  (K : Key) is
         Tmp : Elmt_Ptr;

      begin
         Tmp := Tab.Get (K);

         if Tmp /= null then
            Tab.Remove (K);
            Free (Tmp);
         end if;
      end Remove;

      -----------
      -- Reset --
      -----------

      procedure Reset is
         E1, E2 : Elmt_Ptr;

      begin
         E1 := Tab.Get_First;
         while E1 /= null loop
            E2 := Tab.Get_Next;
            Free (E1);
            E1 := E2;
         end loop;

         Tab.Reset;
      end Reset;

      ---------
      -- Set --
      ---------

      procedure Set (K : Key; E : Element) is
         Tmp : constant Elmt_Ptr := Tab.Get (K);
      begin
         if Tmp = null then
            Tab.Set (new Element_Wrapper'(K, E, null));
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

   ----------
   -- Hash --
   ----------

   function Hash (Key : String) return Header_Num is
      type Uns is mod 2 ** 32;

      function Hash_Fun is
         new System.String_Hash.Hash (Character, String, Uns);

   begin
      return Header_Num'First +
        Header_Num'Base (Hash_Fun (Key) mod Header_Num'Range_Length);
   end Hash;

end System.HTable;
