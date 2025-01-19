------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--  G N A T . T R A C E B A C K . S Y M B O L I C . M O D U L E _ N A M E   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2012-2025, AdaCore                     --
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

--  This is the GNU/Linux specific version of this package

with Interfaces.C; use Interfaces.C;

separate (System.Traceback.Symbolic)

package body Module_Name is

   pragma Linker_Options ("-ldl");

   function Is_Shared_Lib (Base : Address) return Boolean;
   --  Returns True if a shared library

   -------------------
   -- Is_Shared_Lib --
   -------------------

   function Is_Shared_Lib (Base : Address) return Boolean is
      EI_NIDENT : constant := 16;
      type u16 is mod 2 ** 16;

      --  Just declare the needed header information, we just need to read the
      --  type encoded in the second field.

      type Elf32_Ehdr is record
         e_ident : char_array (1 .. EI_NIDENT);
         e_type  : u16;
      end record;

      ET_DYN : constant := 3; -- A shared lib if e_type = ET_DYN

      Header : Elf32_Ehdr;
      pragma Import (Ada, Header);
      --  Suppress initialization in Normalized_Scalars mode
      for Header'Address use Base;

   begin
      return Header.e_type = ET_DYN;
   exception
      when others =>
         return False;
   end Is_Shared_Lib;

   ---------------------------------
   -- Build_Cache_For_All_Modules --
   ---------------------------------

   procedure Build_Cache_For_All_Modules is
      type link_map;
      type link_map_acc is access all link_map;
      pragma Convention (C, link_map_acc);

      type link_map is record
         l_addr : aliased Address;
         --  Base address of the shared object

         l_name : aliased Address;
         --  Null-terminated absolute file name

         l_ld   : aliased Address;
         --  Dynamic section

         l_next, l_prev : aliased link_map_acc;
         --  Chain
      end record;
      pragma Convention (C, link_map);

      type r_debug_state is (RT_CONSISTENT, RT_ADD, RT_DELETE);
      pragma Convention (C, r_debug_state);
      pragma Unreferenced (RT_CONSISTENT, RT_ADD, RT_DELETE);

      type r_debug_type is record
         r_version : aliased int;
         r_map     : aliased link_map_acc;
         r_brk     : aliased Address;
         r_state   : aliased r_debug_state;
         r_ldbase  : aliased Address;
      end record;
      pragma Convention (C, r_debug_type);

      r_debug : r_debug_type;
      pragma Import (C, r_debug, "_r_debug");

      lm : link_map_acc;
   begin
      lm := r_debug.r_map;
      while lm /= null loop
         if Big_String_Conv.To_Pointer (lm.l_name) (1) /= ASCII.NUL then
            --  Discard non-file (like the executable itself or the gate).
            Add_Module_To_Cache (Value (lm.l_name), lm.l_addr);
         end if;
         lm := lm.l_next;
      end loop;
   end Build_Cache_For_All_Modules;

   ---------
   -- Get --
   ---------

   --  The principle is:

   --  1. We get information about the module containing the address.

   --  2. We check whether the module is a shared library.

   --  3. For shared libraries, we return the non-relocated address (so
   --     the absolute address in the shared library).

   --  4. We also return the full pathname of the module containing this
   --     address.

   function Get
     (Addr      : System.Address;
      Load_Addr : access System.Address) return String
   is
      --  Dl_info record for Linux, used to get sym reloc offset

      type Dl_info is record
         dli_fname : System.Address;
         dli_fbase : System.Address;
         dli_sname : System.Address;
         dli_saddr : System.Address;
      end record;

      function dladdr
        (addr : System.Address;
         info : not null access Dl_info) return int;
      pragma Import (C, dladdr, "dladdr");
      --  This is a Linux extension and not POSIX

      info : aliased Dl_info;

   begin
      Load_Addr.all := System.Null_Address;

      if dladdr (Addr, info'Access) /= 0 then

         --  If we have a shared library we need to adjust the address to
         --  be relative to the base address of the library.

         if Is_Shared_Lib (info.dli_fbase) then
            Load_Addr.all := info.dli_fbase;
         end if;

         return Value (info.dli_fname);

      --  Not found, fallback to executable name

      else
         return "";
      end if;

   exception
      when others =>
         return "";
   end Get;

   ------------------
   -- Is_Supported --
   ------------------

   function Is_Supported return Boolean is
   begin
      return True;
   end Is_Supported;

end Module_Name;
