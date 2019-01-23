------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                            G N A T . S E T S                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                        Copyright (C) 2018-2019, AdaCore                  --
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

package body GNAT.Sets is

   --------------------
   -- Membership_Set --
   --------------------

   package body Membership_Set is

      --------------
      -- Contains --
      --------------

      function Contains (S : Instance; Elem : Element_Type) return Boolean is
      begin
         return Hashed_Set.Get (Hashed_Set.Instance (S), Elem);
      end Contains;

      ------------
      -- Create --
      ------------

      function Create (Initial_Size : Positive) return Instance is
      begin
         return Instance (Hashed_Set.Create (Initial_Size));
      end Create;

      ------------
      -- Delete --
      ------------

      procedure Delete (S : Instance; Elem : Element_Type) is
      begin
         Hashed_Set.Delete (Hashed_Set.Instance (S), Elem);
      end Delete;

      -------------
      -- Destroy --
      -------------

      procedure Destroy (S : in out Instance) is
      begin
         Hashed_Set.Destroy (Hashed_Set.Instance (S));
      end Destroy;

      --------------
      -- Has_Next --
      --------------

      function Has_Next (Iter : Iterator) return Boolean is
      begin
         return Hashed_Set.Has_Next (Hashed_Set.Iterator (Iter));
      end Has_Next;

      ------------
      -- Insert --
      ------------

      procedure Insert (S : Instance; Elem : Element_Type) is
      begin
         Hashed_Set.Put (Hashed_Set.Instance (S), Elem, True);
      end Insert;

      --------------
      -- Is_Empty --
      --------------

      function Is_Empty (S : Instance) return Boolean is
      begin
         return Hashed_Set.Is_Empty (Hashed_Set.Instance (S));
      end Is_Empty;

      -------------
      -- Iterate --
      -------------

      function Iterate (S : Instance) return Iterator is
      begin
         return Iterator (Hashed_Set.Iterate (Hashed_Set.Instance (S)));
      end Iterate;

      ----------
      -- Next --
      ----------

      procedure Next (Iter : in out Iterator; Elem : out Element_Type) is
      begin
         Hashed_Set.Next (Hashed_Set.Iterator (Iter), Elem);
      end Next;

      ----------
      -- Size --
      ----------

      function Size (S : Instance) return Natural is
      begin
         return Hashed_Set.Size (Hashed_Set.Instance (S));
      end Size;
   end Membership_Set;

end GNAT.Sets;
