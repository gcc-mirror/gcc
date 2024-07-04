------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                  G N A T . B I N D _ E N V I R O N M E N T               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2015-2024, Free Software Foundation, Inc.      --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by AdaCore.                        --
--                                                                          --
------------------------------------------------------------------------------

with System;

package body GNAT.Bind_Environment is

   ---------
   -- Get --
   ---------

   function Get (Key : String) return String is
      use type System.Address;

      Bind_Env_Addr : constant System.Address;
      pragma Import (C, Bind_Env_Addr, "__gl_bind_env_addr");
      --  Variable provided by init.c/s-init.ads, and initialized by
      --  the binder generated file.

      Bind_Env : String (Positive);
      for Bind_Env'Address use Bind_Env_Addr;
      pragma Import (Ada, Bind_Env);
      --  Import Bind_Env string from binder file. Note that we import
      --  it here as a string with maximum boundaries. The "real" end
      --  of the string is indicated by a NUL byte.

      Index, KLen, VLen : Integer;

   begin
      if Bind_Env_Addr = System.Null_Address then
         return "";
      end if;

      Index := Bind_Env'First;
      loop
         --  Index points to key length

         VLen := 0;
         KLen := Character'Pos (Bind_Env (Index));
         exit when KLen = 0;

         Index := Index + KLen + 1;

         --  Index points to value length

         VLen := Character'Pos (Bind_Env (Index));
         exit when Bind_Env (Index - KLen .. Index - 1) = Key;

         Index := Index + VLen + 1;
      end loop;

      return Bind_Env (Index + 1 .. Index + VLen);
   end Get;

end GNAT.Bind_Environment;
