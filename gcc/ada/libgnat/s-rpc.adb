------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                           S Y S T E M . R P C                            --
--                                                                          --
--                                 B o d y                                  --
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

--  Note: this is a dummy implementation which does not support distribution.
--  All the bodies but one therefore raise an exception as defined below.
--  Establish_RPC_Receiver is callable, so that the ACVC scripts can simulate
--  the presence of a master partition to run a test which is otherwise not
--  distributed.

--  The GLADE distribution package includes a replacement for this file

package body System.RPC is

   CRLF : constant String := ASCII.CR & ASCII.LF;

   Msg : constant String :=
           CRLF & "Distribution support not installed in your environment" &
           CRLF & "For information on GLADE, contact Ada Core Technologies";

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream : in out Params_Stream_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
   begin
      raise Program_Error with Msg;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : in out Params_Stream_Type;
      Item   : Ada.Streams.Stream_Element_Array)
   is
   begin
      raise Program_Error with Msg;
   end Write;

   ------------
   -- Do_RPC --
   ------------

   procedure Do_RPC
     (Partition : Partition_ID;
      Params    : access Params_Stream_Type;
      Result    : access Params_Stream_Type)
   is
   begin
      raise Program_Error with Msg;
   end Do_RPC;

   ------------
   -- Do_APC --
   ------------

   procedure Do_APC
     (Partition : Partition_ID;
      Params    : access Params_Stream_Type)
   is
   begin
      raise Program_Error with Msg;
   end Do_APC;

   ----------------------------
   -- Establish_RPC_Receiver --
   ----------------------------

   procedure Establish_RPC_Receiver
     (Partition : Partition_ID;
      Receiver  : RPC_Receiver)
   is
      pragma Unreferenced (Partition, Receiver);
   begin
      null;
   end Establish_RPC_Receiver;

end System.RPC;
