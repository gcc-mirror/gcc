------------------------------------------------------------------------------
--                                                                          --
--                        GNAT RUN-TIME COMPONENTS                          --
--                                                                          --
--          A D A . S E Q U E N T I A L _ I O . C _ S T R E A M S           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.1.16.1 $
--                                                                          --
--          Copyright (C) 1992-1998 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
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

with Interfaces.C_Streams; use Interfaces.C_Streams;
with System.File_IO;
with System.File_Control_Block;
with System.Sequential_IO;
with Unchecked_Conversion;

package body Ada.Sequential_IO.C_Streams is

   package FIO renames System.File_IO;
   package FCB renames System.File_Control_Block;
   package SIO renames System.Sequential_IO;

   subtype AP is FCB.AFCB_Ptr;

   function To_FCB is new Unchecked_Conversion (File_Mode, FCB.File_Mode);

   --------------
   -- C_Stream --
   --------------

   function C_Stream (F : File_Type) return FILEs is
   begin
      FIO.Check_File_Open (AP (F));
      return F.Stream;
   end C_Stream;

   ----------
   -- Open --
   ----------

   procedure Open
     (File     : in out File_Type;
      Mode     : in File_Mode;
      C_Stream : in FILEs;
      Form     : in String := "")
   is
      File_Control_Block : SIO.Sequential_AFCB;

   begin
      FIO.Open (File_Ptr  => AP (File),
                Dummy_FCB => File_Control_Block,
                Mode      => To_FCB (Mode),
                Name      => "",
                Form      => Form,
                Amethod   => 'Q',
                Creat     => False,
                Text      => False,
                C_Stream  => C_Stream);
   end Open;

end Ada.Sequential_IO.C_Streams;
