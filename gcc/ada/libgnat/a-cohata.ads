------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--            A D A . C O N T A I N E R S . H A S H _ T A B L E S           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2004-2023, Free Software Foundation, Inc.         --
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
-- This unit was originally developed by Matthew J Heaney.                  --
------------------------------------------------------------------------------

--  This package declares the hash-table type used to implement hashed
--  containers.

with Ada.Containers.Helpers;

package Ada.Containers.Hash_Tables is
   pragma Pure;
   --  Declare Pure so this can be imported by Remote_Types packages

   generic
      type Node_Type (<>) is limited private;

      type Node_Access is access Node_Type;

   package Generic_Hash_Table_Types is

      type Buckets_Type is array (Hash_Type range <>) of Node_Access;

      type Buckets_Access is access all Buckets_Type;
      for Buckets_Access'Storage_Size use 0;
      --  Storage_Size of zero so this package can be Pure

      type Hash_Table_Type is tagged record
         Buckets : Buckets_Access := null;
         Length  : Count_Type := 0;
         TC      : aliased Helpers.Tamper_Counts;
      end record;

      package Implementation is new Helpers.Generic_Implementation;
   end Generic_Hash_Table_Types;

   generic
      type Node_Type is private;
   package Generic_Bounded_Hash_Table_Types is

      type Nodes_Type is array (Count_Type range <>) of Node_Type;
      type Buckets_Type is array (Hash_Type range <>) of Count_Type;

      type Hash_Table_Type
        (Capacity : Count_Type;
         Modulus  : Hash_Type) is
      tagged record
         Length  : Count_Type                  := 0;
         TC      : aliased Helpers.Tamper_Counts;
         Free    : Count_Type'Base             := -1;
         Nodes   : Nodes_Type (1 .. Capacity);
         Buckets : Buckets_Type (1 .. Modulus) := [others => 0];
      end record;

      package Implementation is new Helpers.Generic_Implementation;
   end Generic_Bounded_Hash_Table_Types;

   generic
      type Node_Type is private;
   package Generic_Formal_Hash_Table_Types is

      type Nodes_Type is array (Count_Type range <>) of Node_Type;
      type Buckets_Type is array (Hash_Type range <>) of Count_Type;

      type Hash_Table_Type
        (Capacity : Count_Type;
         Modulus  : Hash_Type) is
      record
         Length  : Count_Type                  := 0;
         Free    : Count_Type'Base             := -1;
         Nodes   : Nodes_Type (1 .. Capacity);
         Buckets : Buckets_Type (1 .. Modulus) := [others => 0];
      end record;

   end Generic_Formal_Hash_Table_Types;

end Ada.Containers.Hash_Tables;
