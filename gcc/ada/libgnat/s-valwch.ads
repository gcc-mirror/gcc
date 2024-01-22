------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . V A L _ W C H A R                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2024, Free Software Foundation, Inc.         --
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

--  Processing for Wide_[Wide_]Value attribute

with System.WCh_Con;

package System.Val_WChar is
   pragma Preelaborate;

   function Value_Wide_Character
     (Str : String;
      EM  : System.WCh_Con.WC_Encoding_Method) return Wide_Character;
   --  Computes Wide_Character'Value (Str). The parameter EM is the encoding
   --  method used for any Wide_Character sequences in Str. Note that brackets
   --  notation is always permitted.

   function Value_Wide_Wide_Character
     (Str : String;
      EM  : System.WCh_Con.WC_Encoding_Method) return Wide_Wide_Character;
   --  Computes Wide_Character'Value (Str). The parameter EM is the encoding
   --  method used for any wide_character sequences in Str. Note that brackets
   --  notation is always permitted.

end System.Val_WChar;
