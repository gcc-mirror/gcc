------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                 S Y S T E M . S E Q U E N T I A L _ I O                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 1992-2006, Free Software Foundation, Inc.        --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains the declaration of the control block used for
--  Seqential_IO. This must be declared at the outer library level. It also
--  contains code that is shared between instances of Sequential_IO.

with System.File_Control_Block;
with Ada.Streams;

package System.Sequential_IO is

   package FCB renames System.File_Control_Block;

   type Sequential_AFCB is new FCB.AFCB with null record;
   --  No additional fields required for Sequential_IO

   function AFCB_Allocate
     (Control_Block : Sequential_AFCB) return FCB.AFCB_Ptr;

   procedure AFCB_Close (File : access Sequential_AFCB);
   procedure AFCB_Free  (File : access Sequential_AFCB);

   procedure Read
     (File : in out Sequential_AFCB;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);
   --  Required overriding of Read, not actually used for Sequential_IO

   procedure Write
     (File : in out Sequential_AFCB;
      Item : Ada.Streams.Stream_Element_Array);
   --  Required overriding of Write, not actually used for Sequential_IO

   type File_Type is access all Sequential_AFCB;
   --  File_Type in individual instantiations is derived from this type

   procedure Create
     (File : in out File_Type;
      Mode : FCB.File_Mode := FCB.Out_File;
      Name : String := "";
      Form : String := "");

   procedure Open
     (File : in out File_Type;
      Mode : FCB.File_Mode;
      Name : String;
      Form : String := "");

end System.Sequential_IO;
