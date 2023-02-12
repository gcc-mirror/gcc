------------------------------------------------------------------------------
--                                                                          --
--                          GNAT RUN-TIME COMPONENTS                        --
--                                                                          --
--               S Y S T E M . V E R S I O N _ C O N T R O L                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2023, Free Software Foundation, Inc.         --
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

--  This module contains the runtime routine for implementation of the
--  Version and Body_Version attributes, as well as the string type that
--  is returned as a result of using these attributes.

with System.Unsigned_Types;

package System.Version_Control is
   pragma Pure;

   subtype Version_String is String (1 .. 8);
   --  Eight character string returned by Get_version_String

   function Get_Version_String
     (V : System.Unsigned_Types.Unsigned)
      return Version_String;
   --  The version information in the executable file is stored as unsigned
   --  integers. This routine converts the unsigned integer into an eight
   --  character string containing its hexadecimal digits (with lower case
   --  letters).

end System.Version_Control;
