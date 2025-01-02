------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--             S Y S T E M . S T R E A M _ A T T R I B U T E S              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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

with Ada.IO_Exceptions;
with Ada.Streams; use Ada.Streams;
with Ada.Unchecked_Conversion;
with System.Stream_Attributes.XDR;

package body System.Stream_Attributes is

   XDR_Stream : constant Integer;
   pragma Import (C, XDR_Stream, "__gl_xdr_stream");
   --  This imported value is used to determine whether the build had the
   --  binder switch "-xdr" present which enables XDR streaming and sets this
   --  flag to 1.

   function XDR_Support return Boolean is (XDR_Stream = 1);
   pragma Inline (XDR_Support);
   --  Return True if XDR streaming should be used. Note that 128-bit integers
   --  are not supported by the XDR protocol and will raise Device_Error.

   Err : exception renames Ada.IO_Exceptions.End_Error;
   --  Exception raised if insufficient data read (note that the RM implies
   --  that Data_Error might be the appropriate choice, but AI95-00132
   --  decides with a binding interpretation that End_Error is preferred).

   SU : constant := System.Storage_Unit;

   subtype SEA is Ada.Streams.Stream_Element_Array;
   subtype SEO is Ada.Streams.Stream_Element_Offset;

   generic function UC renames Ada.Unchecked_Conversion;

   --  Subtypes used to define Stream_Element_Array values that map
   --  into the elementary types, using unchecked conversion.

   Thin_Pointer_Size : constant := System.Address'Size;
   Fat_Pointer_Size  : constant := System.Address'Size * 2;

   subtype S_AD   is SEA (1 .. (Fat_Pointer_Size              + SU - 1) / SU);
   subtype S_AS   is SEA (1 .. (Thin_Pointer_Size             + SU - 1) / SU);
   subtype S_B    is SEA (1 .. (Boolean'Size                  + SU - 1) / SU);
   subtype S_C    is SEA (1 .. (Character'Size                + SU - 1) / SU);
   subtype S_F    is SEA (1 .. (Float'Size                    + SU - 1) / SU);
   subtype S_I    is SEA (1 .. (Integer'Size                  + SU - 1) / SU);
   subtype S_I24  is SEA (1 .. (Integer_24'Size               + SU - 1) / SU);
   subtype S_LF   is SEA (1 .. (Long_Float'Size               + SU - 1) / SU);
   subtype S_LI   is SEA (1 .. (Long_Integer'Size             + SU - 1) / SU);
   subtype S_LLF  is SEA (1 .. (Long_Long_Float'Size          + SU - 1) / SU);
   subtype S_LLI  is SEA (1 .. (Long_Long_Integer'Size        + SU - 1) / SU);
   subtype S_LLLI is SEA (1 .. (Long_Long_Long_Integer'Size   + SU - 1) / SU);
   subtype S_LLLU is
                  SEA (1 .. (UST.Long_Long_Long_Unsigned'Size + SU - 1) / SU);
   subtype S_LLU  is SEA (1 .. (UST.Long_Long_Unsigned'Size   + SU - 1) / SU);
   subtype S_LU   is SEA (1 .. (UST.Long_Unsigned'Size        + SU - 1) / SU);
   subtype S_SF   is SEA (1 .. (Short_Float'Size              + SU - 1) / SU);
   subtype S_SI   is SEA (1 .. (Short_Integer'Size            + SU - 1) / SU);
   subtype S_SSI  is SEA (1 .. (Short_Short_Integer'Size      + SU - 1) / SU);
   subtype S_SSU  is SEA (1 .. (UST.Short_Short_Unsigned'Size + SU - 1) / SU);
   subtype S_SU   is SEA (1 .. (UST.Short_Unsigned'Size       + SU - 1) / SU);
   subtype S_U    is SEA (1 .. (UST.Unsigned'Size             + SU - 1) / SU);
   subtype S_U24  is SEA (1 .. (Unsigned_24'Size              + SU - 1) / SU);
   subtype S_WC   is SEA (1 .. (Wide_Character'Size           + SU - 1) / SU);
   subtype S_WWC  is SEA (1 .. (Wide_Wide_Character'Size      + SU - 1) / SU);

   --  Unchecked conversions from the elementary type to the stream type

   function From_AD   is new UC (Fat_Pointer,                 S_AD);
   function From_AS   is new UC (Thin_Pointer,                S_AS);
   function From_F    is new UC (Float,                       S_F);
   function From_I    is new UC (Integer,                     S_I);
   function From_I24  is new UC (Integer_24,                  S_I24);
   function From_LF   is new UC (Long_Float,                  S_LF);
   function From_LI   is new UC (Long_Integer,                S_LI);
   function From_LLF  is new UC (Long_Long_Float,             S_LLF);
   function From_LLI  is new UC (Long_Long_Integer,           S_LLI);
   function From_LLLI is new UC (Long_Long_Long_Integer,      S_LLLI);
   function From_LLLU is new UC (UST.Long_Long_Long_Unsigned, S_LLLU);
   function From_LLU  is new UC (UST.Long_Long_Unsigned,      S_LLU);
   function From_LU   is new UC (UST.Long_Unsigned,           S_LU);
   function From_SF   is new UC (Short_Float,                 S_SF);
   function From_SI   is new UC (Short_Integer,               S_SI);
   function From_SSI  is new UC (Short_Short_Integer,         S_SSI);
   function From_SSU  is new UC (UST.Short_Short_Unsigned,    S_SSU);
   function From_SU   is new UC (UST.Short_Unsigned,          S_SU);
   function From_U    is new UC (UST.Unsigned,                S_U);
   function From_U24  is new UC (Unsigned_24,                 S_U24);
   function From_WC   is new UC (Wide_Character,              S_WC);
   function From_WWC  is new UC (Wide_Wide_Character,         S_WWC);

   --  Unchecked conversions from the stream type to elementary type

   function To_AD   is new UC (S_AD,   Fat_Pointer);
   function To_AS   is new UC (S_AS,   Thin_Pointer);
   function To_F    is new UC (S_F,    Float);
   function To_I    is new UC (S_I,    Integer);
   function To_I24  is new UC (S_I24,  Integer_24);
   function To_LF   is new UC (S_LF,   Long_Float);
   function To_LI   is new UC (S_LI,   Long_Integer);
   function To_LLF  is new UC (S_LLF,  Long_Long_Float);
   function To_LLI  is new UC (S_LLI,  Long_Long_Integer);
   function To_LLLI is new UC (S_LLLI, Long_Long_Long_Integer);
   function To_LLLU is new UC (S_LLLU, UST.Long_Long_Long_Unsigned);
   function To_LLU  is new UC (S_LLU,  UST.Long_Long_Unsigned);
   function To_LU   is new UC (S_LU,   UST.Long_Unsigned);
   function To_SF   is new UC (S_SF,   Short_Float);
   function To_SI   is new UC (S_SI,   Short_Integer);
   function To_SSI  is new UC (S_SSI,  Short_Short_Integer);
   function To_SSU  is new UC (S_SSU,  UST.Short_Short_Unsigned);
   function To_SU   is new UC (S_SU,   UST.Short_Unsigned);
   function To_U    is new UC (S_U,    UST.Unsigned);
   function To_U24  is new UC (S_U24,  Unsigned_24);
   function To_WC   is new UC (S_WC,   Wide_Character);
   function To_WWC  is new UC (S_WWC,  Wide_Wide_Character);

   -----------------
   -- Block_IO_OK --
   -----------------

   function Block_IO_OK return Boolean is
   begin
      return not XDR_Support;
   end Block_IO_OK;

   ----------
   -- I_AD --
   ----------

   function I_AD (Stream : not null access RST) return Fat_Pointer is
      T : S_AD;
      L : SEO;

   begin
      if XDR_Support then
         return XDR.I_AD (Stream);
      end if;

      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return To_AD (T);
      end if;
   end I_AD;

   ----------
   -- I_AS --
   ----------

   function I_AS (Stream : not null access RST) return Thin_Pointer is
      T : S_AS;
      L : SEO;

   begin
      if XDR_Support then
         return XDR.I_AS (Stream);
      end if;

      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return To_AS (T);
      end if;
   end I_AS;

   ---------
   -- I_B --
   ---------

   function I_B (Stream : not null access RST) return Boolean is
      T : S_B;
      L : SEO;

   begin
      if XDR_Support then
         return XDR.I_B (Stream);
      end if;

      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return Boolean'Val (T (1));
      end if;
   end I_B;

   ---------
   -- I_C --
   ---------

   function I_C (Stream : not null access RST) return Character is
      T : S_C;
      L : SEO;

   begin
      if XDR_Support then
         return XDR.I_C (Stream);
      end if;

      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return Character'Val (T (1));
      end if;
   end I_C;

   ---------
   -- I_F --
   ---------

   function I_F (Stream : not null access RST) return Float is
      T : S_F;
      L : SEO;

   begin
      if XDR_Support then
         return XDR.I_F (Stream);
      end if;

      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return To_F (T);
      end if;
   end I_F;

   ---------
   -- I_I --
   ---------

   function I_I (Stream : not null access RST) return Integer is
      T : S_I;
      L : SEO;

   begin
      if XDR_Support then
         return XDR.I_I (Stream);
      end if;

      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return To_I (T);
      end if;
   end I_I;

   -----------
   -- I_I24 --
   -----------

   function I_I24 (Stream : not null access RST) return Integer_24 is
      T : S_I24;
      L : SEO;

   begin
      if XDR_Support then
         return XDR.I_I24 (Stream);
      end if;

      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return To_I24 (T);
      end if;
   end I_I24;

   ----------
   -- I_LF --
   ----------

   function I_LF (Stream : not null access RST) return Long_Float is
      T : S_LF;
      L : SEO;

   begin
      if XDR_Support then
         return XDR.I_LF (Stream);
      end if;

      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return To_LF (T);
      end if;
   end I_LF;

   ----------
   -- I_LI --
   ----------

   function I_LI (Stream : not null access RST) return Long_Integer is
      T : S_LI;
      L : SEO;

   begin
      if XDR_Support then
         return XDR.I_LI (Stream);
      end if;

      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return To_LI (T);
      end if;
   end I_LI;

   -----------
   -- I_LLF --
   -----------

   function I_LLF (Stream : not null access RST) return Long_Long_Float is
      T : S_LLF;
      L : SEO;

   begin
      if XDR_Support then
         return XDR.I_LLF (Stream);
      end if;

      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return To_LLF (T);
      end if;
   end I_LLF;

   -----------
   -- I_LLI --
   -----------

   function I_LLI (Stream : not null access RST) return Long_Long_Integer is
      T : S_LLI;
      L : SEO;

   begin
      if XDR_Support then
         return XDR.I_LLI (Stream);
      end if;

      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return To_LLI (T);
      end if;
   end I_LLI;

   ------------
   -- I_LLLI --
   ------------

   function I_LLLI (Stream : not null access RST) return Long_Long_Long_Integer
   is
      T : S_LLLI;
      L : SEO;

   begin
      if XDR_Support then
         raise Ada.IO_Exceptions.Device_Error;
      end if;

      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return To_LLLI (T);
      end if;
   end I_LLLI;

   ------------
   -- I_LLLU --
   ------------

   function I_LLLU
     (Stream : not null access RST) return UST.Long_Long_Long_Unsigned
   is
      T : S_LLLU;
      L : SEO;

   begin
      if XDR_Support then
         raise Ada.IO_Exceptions.Device_Error;
      end if;

      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return To_LLLU (T);
      end if;
   end I_LLLU;

   -----------
   -- I_LLU --
   -----------

   function I_LLU
     (Stream : not null access RST) return UST.Long_Long_Unsigned
   is
      T : S_LLU;
      L : SEO;

   begin
      if XDR_Support then
         return XDR.I_LLU (Stream);
      end if;

      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return To_LLU (T);
      end if;
   end I_LLU;

   ----------
   -- I_LU --
   ----------

   function I_LU (Stream : not null access RST) return UST.Long_Unsigned is
      T : S_LU;
      L : SEO;

   begin
      if XDR_Support then
         return XDR.I_LU (Stream);
      end if;

      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return To_LU (T);
      end if;
   end I_LU;

   ----------
   -- I_SF --
   ----------

   function I_SF (Stream : not null access RST) return Short_Float is
      T : S_SF;
      L : SEO;

   begin
      if XDR_Support then
         return XDR.I_SF (Stream);
      end if;

      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return To_SF (T);
      end if;
   end I_SF;

   ----------
   -- I_SI --
   ----------

   function I_SI (Stream : not null access RST) return Short_Integer is
      T : S_SI;
      L : SEO;

   begin
      if XDR_Support then
         return XDR.I_SI (Stream);
      end if;

      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return To_SI (T);
      end if;
   end I_SI;

   -----------
   -- I_SSI --
   -----------

   function I_SSI (Stream : not null access RST) return Short_Short_Integer is
      T : S_SSI;
      L : SEO;

   begin
      if XDR_Support then
         return XDR.I_SSI (Stream);
      end if;

      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return To_SSI (T);
      end if;
   end I_SSI;

   -----------
   -- I_SSU --
   -----------

   function I_SSU
     (Stream : not null access RST) return UST.Short_Short_Unsigned
   is
      T : S_SSU;
      L : SEO;

   begin
      if XDR_Support then
         return XDR.I_SSU (Stream);
      end if;

      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return To_SSU (T);
      end if;
   end I_SSU;

   ----------
   -- I_SU --
   ----------

   function I_SU (Stream : not null access RST) return UST.Short_Unsigned is
      T : S_SU;
      L : SEO;

   begin
      if XDR_Support then
         return XDR.I_SU (Stream);
      end if;

      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return To_SU (T);
      end if;
   end I_SU;

   ---------
   -- I_U --
   ---------

   function I_U (Stream : not null access RST) return UST.Unsigned is
      T : S_U;
      L : SEO;

   begin
      if XDR_Support then
         return XDR.I_U (Stream);
      end if;

      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return To_U (T);
      end if;
   end I_U;

   -----------
   -- I_U24 --
   -----------

   function I_U24 (Stream : not null access RST) return Unsigned_24 is
      T : S_U24;
      L : SEO;

   begin
      if XDR_Support then
         return XDR.I_U24 (Stream);
      end if;

      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return To_U24 (T);
      end if;
   end I_U24;

   ----------
   -- I_WC --
   ----------

   function I_WC (Stream : not null access RST) return Wide_Character is
      T : S_WC;
      L : SEO;

   begin
      if XDR_Support then
         return XDR.I_WC (Stream);
      end if;

      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return To_WC (T);
      end if;
   end I_WC;

   -----------
   -- I_WWC --
   -----------

   function I_WWC (Stream : not null access RST) return Wide_Wide_Character is
      T : S_WWC;
      L : SEO;

   begin
      if XDR_Support then
         return XDR.I_WWC (Stream);
      end if;

      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return To_WWC (T);
      end if;
   end I_WWC;

   ----------
   -- W_AD --
   ----------

   procedure W_AD (Stream : not null access RST; Item : Fat_Pointer) is
      T : constant S_AD := From_AD (Item);
   begin
      if XDR_Support then
         XDR.W_AD (Stream, Item);
         return;
      end if;

      Ada.Streams.Write (Stream.all, T);
   end W_AD;

   ----------
   -- W_AS --
   ----------

   procedure W_AS (Stream : not null access RST; Item : Thin_Pointer) is
      T : constant S_AS := From_AS (Item);
   begin
      if XDR_Support then
         XDR.W_AS (Stream, Item);
         return;
      end if;

      Ada.Streams.Write (Stream.all, T);
   end W_AS;

   ---------
   -- W_B --
   ---------

   procedure W_B (Stream : not null access RST; Item : Boolean) is
      T : S_B;
   begin
      if XDR_Support then
         XDR.W_B (Stream, Item);
         return;
      end if;

      T (1) := Boolean'Pos (Item);
      Ada.Streams.Write (Stream.all, T);
   end W_B;

   ---------
   -- W_C --
   ---------

   procedure W_C (Stream : not null access RST; Item : Character) is
      T : S_C;
   begin
      if XDR_Support then
         XDR.W_C (Stream, Item);
         return;
      end if;

      T (1) := Character'Pos (Item);
      Ada.Streams.Write (Stream.all, T);
   end W_C;

   ---------
   -- W_F --
   ---------

   procedure W_F (Stream : not null access RST; Item : Float) is
   begin
      if XDR_Support then
         XDR.W_F (Stream, Item);
         return;
      end if;

      Ada.Streams.Write (Stream.all, From_F (Item));
   end W_F;

   ---------
   -- W_I --
   ---------

   procedure W_I (Stream : not null access RST; Item : Integer) is
   begin
      if XDR_Support then
         XDR.W_I (Stream, Item);
         return;
      end if;

      Ada.Streams.Write (Stream.all, From_I (Item));
   end W_I;

   -----------
   -- W_I24 --
   -----------

   procedure W_I24 (Stream : not null access RST; Item : Integer_24) is
   begin
      if XDR_Support then
         XDR.W_I24 (Stream, Item);
         return;
      end if;

      Ada.Streams.Write (Stream.all, From_I24 (Item));
   end W_I24;

   ----------
   -- W_LF --
   ----------

   procedure W_LF (Stream : not null access RST; Item : Long_Float) is
   begin
      if XDR_Support then
         XDR.W_LF (Stream, Item);
         return;
      end if;

      Ada.Streams.Write (Stream.all, From_LF (Item));
   end W_LF;

   ----------
   -- W_LI --
   ----------

   procedure W_LI (Stream : not null access RST; Item : Long_Integer) is
   begin
      if XDR_Support then
         XDR.W_LI (Stream, Item);
         return;
      end if;

      Ada.Streams.Write (Stream.all, From_LI (Item));
   end W_LI;

   -----------
   -- W_LLF --
   -----------

   procedure W_LLF (Stream : not null access RST; Item : Long_Long_Float) is
   begin
      if XDR_Support then
         XDR.W_LLF (Stream, Item);
         return;
      end if;

      Ada.Streams.Write (Stream.all, From_LLF (Item));
   end W_LLF;

   -----------
   -- W_LLI --
   -----------

   procedure W_LLI (Stream : not null access RST; Item : Long_Long_Integer) is
   begin
      if XDR_Support then
         XDR.W_LLI (Stream, Item);
         return;
      end if;

      Ada.Streams.Write (Stream.all, From_LLI (Item));
   end W_LLI;

   ------------
   -- W_LLLI --
   ------------

   procedure W_LLLI
     (Stream : not null access RST; Item : Long_Long_Long_Integer) is
   begin
      if XDR_Support then
         raise Ada.IO_Exceptions.Device_Error;
      end if;

      Ada.Streams.Write (Stream.all, From_LLLI (Item));
   end W_LLLI;

   ------------
   -- W_LLLU --
   ------------

   procedure W_LLLU
     (Stream : not null access RST; Item : UST.Long_Long_Long_Unsigned)
   is
   begin
      if XDR_Support then
         raise Ada.IO_Exceptions.Device_Error;
      end if;

      Ada.Streams.Write (Stream.all, From_LLLU (Item));
   end W_LLLU;

   -----------
   -- W_LLU --
   -----------

   procedure W_LLU
     (Stream : not null access RST; Item : UST.Long_Long_Unsigned)
   is
   begin
      if XDR_Support then
         XDR.W_LLU (Stream, Item);
         return;
      end if;

      Ada.Streams.Write (Stream.all, From_LLU (Item));
   end W_LLU;

   ----------
   -- W_LU --
   ----------

   procedure W_LU (Stream : not null access RST; Item : UST.Long_Unsigned) is
   begin
      if XDR_Support then
         XDR.W_LU (Stream, Item);
         return;
      end if;

      Ada.Streams.Write (Stream.all, From_LU (Item));
   end W_LU;

   ----------
   -- W_SF --
   ----------

   procedure W_SF (Stream : not null access RST; Item : Short_Float) is
   begin
      if XDR_Support then
         XDR.W_SF (Stream, Item);
         return;
      end if;

      Ada.Streams.Write (Stream.all, From_SF (Item));
   end W_SF;

   ----------
   -- W_SI --
   ----------

   procedure W_SI (Stream : not null access RST; Item : Short_Integer) is
   begin
      if XDR_Support then
         XDR.W_SI (Stream, Item);
         return;
      end if;

      Ada.Streams.Write (Stream.all, From_SI (Item));
   end W_SI;

   -----------
   -- W_SSI --
   -----------

   procedure W_SSI
     (Stream : not null access RST; Item : Short_Short_Integer)
   is
   begin
      if XDR_Support then
         XDR.W_SSI (Stream, Item);
         return;
      end if;

      Ada.Streams.Write (Stream.all, From_SSI (Item));
   end W_SSI;

   -----------
   -- W_SSU --
   -----------

   procedure W_SSU
     (Stream : not null access RST; Item : UST.Short_Short_Unsigned)
   is
   begin
      if XDR_Support then
         XDR.W_SSU (Stream, Item);
         return;
      end if;

      Ada.Streams.Write (Stream.all, From_SSU (Item));
   end W_SSU;

   ----------
   -- W_SU --
   ----------

   procedure W_SU (Stream : not null access RST; Item : UST.Short_Unsigned) is
   begin
      if XDR_Support then
         XDR.W_SU (Stream, Item);
         return;
      end if;

      Ada.Streams.Write (Stream.all, From_SU (Item));
   end W_SU;

   ---------
   -- W_U --
   ---------

   procedure W_U (Stream : not null access RST; Item : UST.Unsigned) is
   begin
      if XDR_Support then
         XDR.W_U (Stream, Item);
         return;
      end if;

      Ada.Streams.Write (Stream.all, From_U (Item));
   end W_U;

   -----------
   -- W_U24 --
   -----------

   procedure W_U24 (Stream : not null access RST; Item : Unsigned_24) is
   begin
      if XDR_Support then
         XDR.W_U24 (Stream, Item);
         return;
      end if;

      Ada.Streams.Write (Stream.all, From_U24 (Item));
   end W_U24;

   ----------
   -- W_WC --
   ----------

   procedure W_WC (Stream : not null access RST; Item : Wide_Character) is
   begin
      if XDR_Support then
         XDR.W_WC (Stream, Item);
         return;
      end if;

      Ada.Streams.Write (Stream.all, From_WC (Item));
   end W_WC;

   -----------
   -- W_WWC --
   -----------

   procedure W_WWC
     (Stream : not null access RST; Item : Wide_Wide_Character)
   is
   begin
      if XDR_Support then
         XDR.W_WWC (Stream, Item);
         return;
      end if;

      Ada.Streams.Write (Stream.all, From_WWC (Item));
   end W_WWC;

end System.Stream_Attributes;
