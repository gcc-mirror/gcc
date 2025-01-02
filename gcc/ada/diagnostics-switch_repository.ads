------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--  D I A G N O S T I C S . D I A G N O S T I C S _ R E P O S I T O R Y     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------
with Erroutc; use Erroutc;

package Diagnostics.Switch_Repository is

   function Get_Switch (Id : Switch_Id) return Switch_Type;

   function Get_Switch (Diag : Diagnostic_Type) return Switch_Type;

   function Get_Switch_Id (E : Error_Msg_Object) return Switch_Id;

   function Get_Switch_Id (Name : String) return Switch_Id;

   procedure Print_Switch_Repository;

end Diagnostics.Switch_Repository;
