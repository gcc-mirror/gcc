------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                           S P A R K _ X R E F S                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2011-2024, Free Software Foundation, Inc.         --
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

with Lib.Xref;
with Output;   use Output;
with Sem_Util; use Sem_Util;

package body SPARK_Xrefs is

   ------------
   -- dspark --
   ------------

   procedure dspark is

      procedure Dump (Index : Nat; AXR : SPARK_Xref_Record);

      procedure Dump_SPARK_Xrefs is new
        Lib.Xref.SPARK_Specific.Iterate_SPARK_Xrefs (Dump);

      ----------
      -- Dump --
      ----------

      procedure Dump (Index : Nat; AXR : SPARK_Xref_Record) is
      begin
         Write_Str  ("  ");
         Write_Int  (Index);
         Write_Char ('.');

         Write_Str  (" Entity = " & Unique_Name (AXR.Entity));
         Write_Str  (" (");
         Write_Int  (Nat (AXR.Entity));
         Write_Str  (")");

         Write_Str  (" Scope = " & Unique_Name (AXR.Ref_Scope));
         Write_Str  (" (");
         Write_Int  (Nat (AXR.Ref_Scope));
         Write_Str  (")");

         Write_Str  (" Ref_Type = '" & AXR.Rtype & "'");

         Write_Eol;
      end Dump;

   --  Start of processing for dspark

   begin
      --  Dump SPARK cross-reference table

      Write_Eol;
      Write_Line ("SPARK Xref Table");
      Write_Line ("----------------");

      Dump_SPARK_Xrefs;

   end dspark;

end SPARK_Xrefs;
