------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--               G N A T . C P P . S T D . T Y P E _ I N F O                --
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

--  This package provides an interface to C++'s std::type_info objects

with System;
with Interfaces.C;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;

--  This unit should be kept private.  We can't run constructors
--  (they're inline), and destructors have to be declared before the
--  public methods because of their position in the virtual method
--  table, so they have to be public as well.  It would be unsafe to
--  run them, since std::type_info objects are typically
--  statically-allocated.  Without out-of-line constructors, deriving
--  from this type would be a challenge.  However, there are some
--  select APIs that we wish to use for exception handling machinery,
--  and others that could be potentially useful in Ada/C++ programs,
--  that are exposed in GNAT.CPP.Std.
private package GNAT.CPP.Std.Type_Info is
   type type_info is abstract tagged limited private;
   pragma Import (CPP, type_info);

   pragma Warnings (Off, "CPP constructor required for type");

   --  Defined inline, no out-of-line version available.
   --  function Constructor (name : Interfaces.C.Strings.chars_ptr)
   --                       return type_info is abstract;
   --  pragma Cpp_Constructor (Constructor, "_ZNSt9type_infoC1EPKc");

   --  Allowing destruction is undesirable, but since these are in
   --  the virtual method table, we have to declare them.
   --  ??? Could we make them private somehow?
   procedure Destructor (this : access type_info);
   pragma Import (CPP, Destructor, "_ZNSt9type_infoD1Ev");
   pragma Machine_Attribute (Destructor, "nothrow");
   procedure Delete_Destructor (This : access type_info);
   pragma Import (CPP, Delete_Destructor, "_ZNSt9type_InfoD0Ev");
   pragma Machine_Attribute (Delete_Destructor, "nothrow");

   --  Defined inline, no out-of-line version available.
   --  Reimplemented in Ada, using Ada types.
   function Name (this : access constant type_info'Class)
                  --  return Interfaces.C.Strings.chars_ptr;
                  return Interfaces.C.Strings.chars_ptr;
   --  pragma Import (CPP, Name, "_ZNKSt9type_info4nameEv");
   pragma Machine_Attribute (Name, "nothrow");

   --  Defined inline, no out-of-line version available.
   --  Reimplemented in Ada, using Ada types.
   function Before (this, that : access constant type_info'Class)
                    --  return Interfaces.C.Extensions.bool;
                    return       Boolean;
   --  pragma Import (CPP, Before, "_ZNKSt9type_info6beforeERKS_");
   pragma Machine_Attribute (Before, "nothrow");

   --  Defined inline, no out-of-line version available.
   --  Reimplemented in Ada, with an Ada interface.
   function Equals (this, that : access constant type_info'Class)
                    --  return Interfaces.C.Extensions.bool;
                    return       Boolean;
   --  pragma Import (CPP, Equals, "_ZNKSt9type_infoeqERKS_");
   pragma Machine_Attribute (Equals, "nothrow");

   --  function "/=" (this : access constant type_info'Class;
   --                  arg : access constant type_info'Class)
   --                 return Interfaces.C.Extensions.bool;
   --  pragma Import (CPP, "/=", "_ZNKSt9type_infoneERKS_");
   --  pragma Machine_Attribute ("/=", "nothrow");

   --  Defined inline, no out-of-line version available.
   --  function Hash_Code (this : access constant type_info'Class)
   --                     return Interfaces.C.size_t;
   --  pragma Import (CPP, Hash_Code, "_ZNKSt9type_info9hash_codeEv");
   --  pragma Machine_Attribute (Hash_Code, "nothrow");

   function Is_Pointer_P (this : access constant type_info)
                          return Interfaces.C.Extensions.bool;
   pragma Import (CPP, Is_Pointer_P,
                  "_ZNKSt9type_info14__is_pointer_pEv");

   function Is_Function_P (this : access constant type_info)
                           return Interfaces.C.Extensions.bool;
   pragma Import (CPP, Is_Function_P,
                  "_ZNKSt9type_info15__is_function_pEv");

   function Do_Catch
     (this          : access constant type_info;
      thrown_type   : access constant type_info'Class;
      thrown_object : in out System.Address;
      outer_count   : Interfaces.C.unsigned)
      return          Interfaces.C.Extensions.bool;
   pragma Import (CPP, Do_Catch,
                  "_ZNKSt9type_info10__do_catchEPKS_PPvj");

   function Do_Upcast
     (this    : access constant type_info;
      target  : access constant type_info'Class;
      obj_ptr : in out System.Address)
      return    Interfaces.C.Extensions.bool;
   pragma Import
     (CPP, Do_Upcast,
      "_ZNKSt9type_info11__do_upcastEPKN10"
        & "__cxxabiv117__class_type_infoEPPv");

private

   type type_info is abstract tagged limited record
      Raw_Name : Interfaces.C.Strings.chars_ptr;
   end record;

end GNAT.CPP.Std.Type_Info;
