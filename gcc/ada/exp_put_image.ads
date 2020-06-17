------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                        E X P _ P U T _ I M A G E                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2020, Free Software Foundation, Inc.           --
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

with Types; use Types;

package Exp_Put_Image is

   --  Routines to build Put_Image calls. See Ada.Strings.Text_Output.Utils and
   --  System.Put_Images for the run-time routines we are generating calls to.

   --  For a call to T'Put_Image, if T is elementary, we expand the code
   --  inline. If T is a tagged type, then Put_Image is a primitive procedure
   --  of T, and can be dispatched to in the class-wide case. For untagged
   --  composite types, we generate a procedure the first time we see a call,
   --  and call it. Subsequent calls call the same procedure. Thus, if there
   --  are calls to T'Put_Image in different units, there will be duplicates;
   --  each unit will get a copy of the T'Put_Image procedure.

   function Enable_Put_Image (Typ : Entity_Id) return Boolean;
   --  True if the predefined Put_Image should be enabled for type T. Put_Image
   --  is always enabled if there is a user-specified one.

   function Build_Put_Image_Profile
     (Loc : Source_Ptr; Typ : Entity_Id) return List_Id;
   --  Builds the parameter profile for Put_Image. This is used for the tagged
   --  case to build the spec for the primitive operation.

   --  In the following Build_... routines, N is the attribute reference node,
   --  from which the procedure to call and the parameters to pass can be
   --  determined.

   function Build_Elementary_Put_Image_Call (N : Node_Id) return Node_Id;
   --  Builds a Put_Image call for an elementary type.

   function Build_String_Put_Image_Call (N : Node_Id) return Node_Id;
   --  Builds a Put_Image call for a standard string type.

   function Build_Protected_Put_Image_Call (N : Node_Id) return Node_Id;
   --  Builds a Put_Image call for a protected type.

   function Build_Task_Put_Image_Call (N : Node_Id) return Node_Id;
   --  Builds a Put_Image call for a task type.

   --  The following routines build the Put_Image procedure for composite
   --  types. Typ is the base type to which the procedure applies (i.e. the
   --  base type of the Put_Image attribute prefix). The returned results are
   --  the declaration and name (entity) of the procedure.

   procedure Build_Array_Put_Image_Procedure
     (Nod  : Node_Id;
      Typ  : Entity_Id;
      Decl : out Node_Id;
      Pnam : out Entity_Id);
   --  Nod provides the Sloc value for the generated code

   procedure Build_Record_Put_Image_Procedure
     (Loc  : Source_Ptr;
      Typ  : Entity_Id;
      Decl : out Node_Id;
      Pnam : out Entity_Id);
   --  Loc is the location of the subprogram declaration

   function Build_Unknown_Put_Image_Call (N : Node_Id) return Node_Id;
   --  Build a call to Put_Image_Unknown

   function Image_Should_Call_Put_Image (N : Node_Id) return Boolean;
   --  True if T'Image should call T'Put_Image. N is the attribute_reference
   --  T'Image.

   function Build_Image_Call (N : Node_Id) return Node_Id;
   --  N is a call to T'Image, and this translates it into the appropriate code
   --  to call T'Put_Image into a buffer and then extract the string from the
   --  buffer.

   procedure Preload_Sink (Compilation_Unit : Node_Id);
   --  Call RTE (RE_Sink) if necessary, to load the packages involved in
   --  Put_Image. We need to do this explicitly, fairly early during
   --  compilation, because otherwise it happens during freezing, which
   --  triggers visibility bugs in generic instantiations.

end Exp_Put_Image;
