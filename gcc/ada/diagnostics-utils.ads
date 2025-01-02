------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     D I A G N O S T I C S . U T I L S                    --
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

package Diagnostics.Utils is

   function Get_Human_Id (D : Diagnostic_Type) return String_Ptr;

   function Sloc_To_String (Sptr : Source_Ptr; Ref : Source_Ptr) return String;
   --  Convert the source pointer to a string and prefix it with the correct
   --  preposition.
   --
   --  * If the location is in one of the standard locations,
   --    then it yields "in package <LOCATION>". The explicit standard
   --    locations are:
   --    * System
   --    * Standard
   --    * Standard.ASCII
   --  * if the location is missing the the sloc yields "at unknown location"
   --  * if the location is in the same file as the current file,
   --    then it yields "at line <line>".
   --  * Otherwise sloc yields "at <file>:<line>:<column>"

   function Sloc_To_String (N : Node_Or_Entity_Id;
                            Ref : Source_Ptr)
                            return String;
   --  Converts the Sloc of the node or entity to a Sloc string.

   function To_String (Sptr : Source_Ptr) return String;
   --  Convert the source pointer to a string of the form: "file:line:column"

   function To_File_Name (Sptr : Source_Ptr) return String;
   --  Converts the file name of the Sptr to a string.

   function Line_To_String (Sptr : Source_Ptr) return String;
   --  Converts the logical line number of the Sptr to a string.

   function Column_To_String (Sptr : Source_Ptr) return String;
   --  Converts the column number of the Sptr to a string. Column values less
   --  than 10 are prefixed with a 0.

   function To_Full_Span (N : Node_Id) return Source_Span;

   function To_String (Id : Diagnostic_Id) return String;
   --  Convert the diagnostic ID to a 4 character string padded with 0-s.

   function To_Name (E : Entity_Id) return String;

   function To_Type_Name (E : Entity_Id) return String;

   function Kind_To_String (D : Diagnostic_Type) return String;

   function Kind_To_String
     (D : Sub_Diagnostic_Type;
      Parent : Diagnostic_Type) return String;

   function Get_Primary_Labeled_Span (Spans : Labeled_Span_List)
                                      return Labeled_Span_Type;

   function Get_Doc_Switch (Diag : Diagnostic_Type) return String;

   function Appears_Before (D1, D2 : Diagnostic_Type) return Boolean;

   function Appears_Before (P1, P2 : Source_Ptr) return Boolean;

   procedure Insert_Based_On_Location
     (List       : Diagnostic_List;
      Diagnostic : Diagnostic_Type);

end Diagnostics.Utils;
