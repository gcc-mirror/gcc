------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                    ADA.STRINGS.TEXT_OUTPUT.BIT_BUCKETS                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2020-2021, Free Software Foundation, Inc.       --
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

package body Ada.Strings.Text_Output.Bit_Buckets is

   type Bit_Bucket_Type is new Sink with null record;
   overriding procedure Full_Method (S : in out Bit_Bucket_Type);
   overriding procedure Flush_Method (S : in out Bit_Bucket_Type);

   The_Bit_Bucket : aliased Bit_Bucket_Type
     (Chunk_Length => Default_Chunk_Length);
   function Bit_Bucket return Sink_Access is (The_Bit_Bucket'Access);

   overriding procedure Full_Method (S : in out Bit_Bucket_Type)
                renames Flush_Method;

   overriding procedure Flush_Method (S : in out Bit_Bucket_Type) is
   begin
      S.Last := 0;
   end Flush_Method;

begin
   The_Bit_Bucket.Indent_Amount := 0;
   The_Bit_Bucket.Cur_Chunk := The_Bit_Bucket.Initial_Chunk'Access;
end Ada.Strings.Text_Output.Bit_Buckets;
