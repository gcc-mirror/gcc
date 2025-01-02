------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         G N A T . C P P . S T D                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2022-2025, AdaCore                     --
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

--  This package may provide an Ada interface for some C++ standard
--  entities in the std namespace.

with GNAT.CPP.Std.Type_Info;
with Ada.Unchecked_Conversion;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body GNAT.CPP.Std is
   ----------------------
   -- To_Type_Info_Ptr --
   ----------------------

   function To_Type_Info_Ptr (S : System.Address) return Type_Info_Ptr is
      function Impl is
         new Ada.Unchecked_Conversion (System.Address, Type_Info_Ptr);
   begin
      return Impl (S);
   end To_Type_Info_Ptr;

   ------------
   --  Name  --
   ------------

   function Name (this : Type_Info_Ptr)
                  return String
   is (Value (this.all.Name));

   ---------------
   --  Before  ---
   ---------------

   function Before (this, that : Type_Info_Ptr)
                    --  return Interfaces.C.Extensions.bool;
                    return       Boolean
   is (this.all.Before (that));

   ---------------
   --  Equals  ---
   ---------------

   function Equals (this, that : Type_Info_Ptr)
                    --  return Interfaces.C.Extensions.bool;
                    return       Boolean
   is (this.all.Equals (that));

   --------------------
   --  Is_Pointer_P  --
   --------------------

   function Is_Pointer_P (this : Type_Info_Ptr)
                          return Interfaces.C.Extensions.bool
   is (this.all.Is_Pointer_P);

   ---------------------
   --  Is_Function_P  --
   ---------------------

   function Is_Function_P (this : Type_Info_Ptr)
                           return Interfaces.C.Extensions.bool
   is (this.all.Is_Function_P);

   ----------------
   --  Do_Catch ---
   ----------------

   function Do_Catch
     (this          : Type_Info_Ptr;
      thrown_type   : Type_Info_Ptr;
      thrown_object : in out System.Address;
      outer_count   : Interfaces.C.unsigned)
      return          Interfaces.C.Extensions.bool
   is (this.all.Do_Catch (thrown_type, thrown_object, outer_count));

   -----------------
   --  Do_Upcast ---
   -----------------

   function Do_Upcast
     (this    : Type_Info_Ptr;
      target  : Type_Info_Ptr;
      obj_ptr : in out System.Address)
      return    Interfaces.C.Extensions.bool
   is (this.all.Do_Upcast (target, obj_ptr));

end GNAT.CPP.Std;
