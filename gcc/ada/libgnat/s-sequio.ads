------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                 S Y S T E M . S E Q U E N T I A L _ I O                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 1992-2017, Free Software Foundation, Inc.        --
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

--  This package contains the declaration of the control block used for
--  Sequential_IO. This must be declared at the outer library level. It also
--  contains code that is shared between instances of Sequential_IO.

with System.File_Control_Block;
with Ada.Streams;

package System.Sequential_IO is

   package FCB renames System.File_Control_Block;

   type Sequential_AFCB is new FCB.AFCB with null record;
   --  No additional fields required for Sequential_IO

   function AFCB_Allocate
     (Control_Block : Sequential_AFCB) return FCB.AFCB_Ptr;

   procedure AFCB_Close (File : not null access Sequential_AFCB);
   procedure AFCB_Free  (File : not null access Sequential_AFCB);

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
