------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . M M A P . U N I X                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2007-2019, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
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

--  Declaration of off_t/mmap/munmap. This particular implementation
--  supposes off_t is long.

with System.OS_Lib;
with Interfaces.C;

package System.Mmap.Unix is

   type Mmap_Prot is new Interfaces.C.int;
--     PROT_NONE  : constant Mmap_Prot := 16#00#;
--     PROT_EXEC  : constant Mmap_Prot := 16#04#;
   PROT_READ  : constant Mmap_Prot := 16#01#;
   PROT_WRITE : constant Mmap_Prot := 16#02#;

   type Mmap_Flags is new Interfaces.C.int;
--     MAP_NONE    : constant Mmap_Flags := 16#00#;
--     MAP_FIXED   : constant Mmap_Flags := 16#10#;
   MAP_SHARED  : constant Mmap_Flags := 16#01#;
   MAP_PRIVATE : constant Mmap_Flags := 16#02#;

   type off_t is new Long_Integer;

   function Mmap (Start  : Address := Null_Address;
                  Length : Interfaces.C.size_t;
                  Prot   : Mmap_Prot := PROT_READ;
                  Flags  : Mmap_Flags := MAP_PRIVATE;
                  Fd     : System.OS_Lib.File_Descriptor;
                  Offset : off_t) return Address;
   pragma Import (C, Mmap, "mmap");

   function Munmap (Start  : Address;
                    Length : Interfaces.C.size_t) return Integer;
   pragma Import (C, Munmap, "munmap");

   function Is_Mapping_Available return Boolean is (True);
   --  Wheter memory mapping is actually available on this system. It is an
   --  error to use Create_Mapping and Dispose_Mapping if this is False.
end System.Mmap.Unix;
