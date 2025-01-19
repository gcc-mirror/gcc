------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         G N A T . C P P . S T D                          --
--                                                                          --
--                                 S p e c                                  --
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

limited private with GNAT.CPP.Std.Type_Info;
with System; use System;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions; use Interfaces.C.Extensions;

package GNAT.CPP.Std is
   type Type_Info_Ptr is private;
   --  This type stands for C++ std::type_info *.

   No_Type_Info : constant Type_Info_Ptr;

   function To_Type_Info_Ptr (S : System.Address) return Type_Info_Ptr;
   --  Return an opaque Type_Info_Ptr referencing the std::type_info
   --  object presumed to be at S.

   function Name (this : Type_Info_Ptr)
                  --  return Interfaces.C.Strings.chars_ptr;
                  return String;
   --  Exposed std::type_info member function.  ??? Would it ever be
   --  desirable to get direct access to the internal chars_ptr?

   function Before (this, that : Type_Info_Ptr)
                    --  return Interfaces.C.Extensions.bool;
                    return       Boolean;
   --  Exposed std::type_info member function.

   function Equals (this, that : Type_Info_Ptr)
                    --  return Interfaces.C.Extensions.bool;
                    return       Boolean;
   --  Exposed std::type_info member function.

   function Is_Pointer_P (this : Type_Info_Ptr)
                          return Interfaces.C.Extensions.bool;
   --  Exposed std::type_info member function.

   function Is_Function_P (this : Type_Info_Ptr)
                           return Interfaces.C.Extensions.bool;
   --  Exposed std::type_info member function.

   function Do_Catch
     (this          : Type_Info_Ptr;
      thrown_type   : Type_Info_Ptr;
      thrown_object : in out System.Address;
      outer_count   : Interfaces.C.unsigned)
      return          Interfaces.C.Extensions.bool;
   --  Exposed std::type_info member function.

   function Do_Upcast
     (this    : Type_Info_Ptr;
      target  : Type_Info_Ptr;
      obj_ptr : in out System.Address)
      return    Interfaces.C.Extensions.bool;
   --  Exposed std::type_info member function.

private

   type Type_Info_Ptr is access constant Type_Info.type_info'Class;
   pragma No_Strict_Aliasing (Type_Info_Ptr);
   pragma No_Heap_Finalization (Type_Info_Ptr);

   No_Type_Info : constant Type_Info_Ptr := null;

end GNAT.CPP.Std;
