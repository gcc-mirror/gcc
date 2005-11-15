------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--             S Y S T E M . S T R E A M _ A T T R I B U T E S              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2005, Free Software Foundation, Inc.         --
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

with Ada.IO_Exceptions;
with Ada.Streams; use Ada.Streams;
with Unchecked_Conversion;

package body System.Stream_Attributes is

   Err : exception renames Ada.IO_Exceptions.End_Error;
   --  Exception raised if insufficient data read (note that the RM implies
   --  that Data_Error might be the appropriate choice, but AI95-00132
   --  decides with a binding interpretation that End_Error is preferred).

   SU : constant := System.Storage_Unit;

   subtype SEA is Ada.Streams.Stream_Element_Array;
   subtype SEO is Ada.Streams.Stream_Element_Offset;

   generic function UC renames Unchecked_Conversion;

   --  Subtypes used to define Stream_Element_Array values that map
   --  into the elementary types, using unchecked conversion.

   Thin_Pointer_Size : constant := System.Address'Size;
   Fat_Pointer_Size  : constant := System.Address'Size * 2;

   subtype S_AD  is SEA (1 .. (Fat_Pointer_Size              + SU - 1) / SU);
   subtype S_AS  is SEA (1 .. (Thin_Pointer_Size             + SU - 1) / SU);
   subtype S_B   is SEA (1 .. (Boolean'Size                  + SU - 1) / SU);
   subtype S_C   is SEA (1 .. (Character'Size                + SU - 1) / SU);
   subtype S_F   is SEA (1 .. (Float'Size                    + SU - 1) / SU);
   subtype S_I   is SEA (1 .. (Integer'Size                  + SU - 1) / SU);
   subtype S_LF  is SEA (1 .. (Long_Float'Size               + SU - 1) / SU);
   subtype S_LI  is SEA (1 .. (Long_Integer'Size             + SU - 1) / SU);
   subtype S_LLF is SEA (1 .. (Long_Long_Float'Size          + SU - 1) / SU);
   subtype S_LLI is SEA (1 .. (Long_Long_Integer'Size        + SU - 1) / SU);
   subtype S_LLU is SEA (1 .. (UST.Long_Long_Unsigned'Size   + SU - 1) / SU);
   subtype S_LU  is SEA (1 .. (UST.Long_Unsigned'Size        + SU - 1) / SU);
   subtype S_SF  is SEA (1 .. (Short_Float'Size              + SU - 1) / SU);
   subtype S_SI  is SEA (1 .. (Short_Integer'Size            + SU - 1) / SU);
   subtype S_SSI is SEA (1 .. (Short_Short_Integer'Size      + SU - 1) / SU);
   subtype S_SSU is SEA (1 .. (UST.Short_Short_Unsigned'Size + SU - 1) / SU);
   subtype S_SU  is SEA (1 .. (UST.Short_Unsigned'Size       + SU - 1) / SU);
   subtype S_U   is SEA (1 .. (UST.Unsigned'Size             + SU - 1) / SU);
   subtype S_WC  is SEA (1 .. (Wide_Character'Size           + SU - 1) / SU);

   --  Unchecked conversions from the elementary type to the stream type

   function From_AD  is new UC (Fat_Pointer,              S_AD);
   function From_AS  is new UC (Thin_Pointer,             S_AS);
   function From_F   is new UC (Float,                    S_F);
   function From_I   is new UC (Integer,                  S_I);
   function From_LF  is new UC (Long_Float,               S_LF);
   function From_LI  is new UC (Long_Integer,             S_LI);
   function From_LLF is new UC (Long_Long_Float,          S_LLF);
   function From_LLI is new UC (Long_Long_Integer,        S_LLI);
   function From_LLU is new UC (UST.Long_Long_Unsigned,   S_LLU);
   function From_LU  is new UC (UST.Long_Unsigned,        S_LU);
   function From_SF  is new UC (Short_Float,              S_SF);
   function From_SI  is new UC (Short_Integer,            S_SI);
   function From_SSI is new UC (Short_Short_Integer,      S_SSI);
   function From_SSU is new UC (UST.Short_Short_Unsigned, S_SSU);
   function From_SU  is new UC (UST.Short_Unsigned,       S_SU);
   function From_U   is new UC (UST.Unsigned,             S_U);
   function From_WC  is new UC (Wide_Character,           S_WC);

   --  Unchecked conversions from the stream type to elementary type

   function To_AD  is new UC (S_AD,  Fat_Pointer);
   function To_AS  is new UC (S_AS,  Thin_Pointer);
   function To_F   is new UC (S_F,   Float);
   function To_I   is new UC (S_I,   Integer);
   function To_LF  is new UC (S_LF,  Long_Float);
   function To_LI  is new UC (S_LI,  Long_Integer);
   function To_LLF is new UC (S_LLF, Long_Long_Float);
   function To_LLI is new UC (S_LLI, Long_Long_Integer);
   function To_LLU is new UC (S_LLU, UST.Long_Long_Unsigned);
   function To_LU  is new UC (S_LU,  UST.Long_Unsigned);
   function To_SF  is new UC (S_SF,  Short_Float);
   function To_SI  is new UC (S_SI,  Short_Integer);
   function To_SSI is new UC (S_SSI, Short_Short_Integer);
   function To_SSU is new UC (S_SSU, UST.Short_Short_Unsigned);
   function To_SU  is new UC (S_SU,  UST.Short_Unsigned);
   function To_U   is new UC (S_U,   UST.Unsigned);
   function To_WC  is new UC (S_WC,  Wide_Character);

   ----------
   -- I_AD --
   ----------

   function I_AD (Stream : not null access RST) return Fat_Pointer is
      T : S_AD;
      L : SEO;

   begin
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
      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return To_I (T);
      end if;
   end I_I;

   ----------
   -- I_LF --
   ----------

   function I_LF (Stream : not null access RST) return Long_Float is
      T : S_LF;
      L : SEO;

   begin
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
      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return To_LLI (T);
      end if;
   end I_LLI;

   -----------
   -- I_LLU --
   -----------

   function I_LLU
     (Stream : not null access RST) return UST.Long_Long_Unsigned
   is
      T : S_LLU;
      L : SEO;

   begin
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
      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return To_U (T);
      end if;
   end I_U;

   ----------
   -- I_WC --
   ----------

   function I_WC (Stream : not null access RST) return Wide_Character is
      T : S_WC;
      L : SEO;

   begin
      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return To_WC (T);
      end if;
   end I_WC;

   ----------
   -- W_AD --
   ----------

   procedure W_AD (Stream : not null access RST; Item : in Fat_Pointer) is
      T : constant S_AD := From_AD (Item);

   begin
      Ada.Streams.Write (Stream.all, T);
   end W_AD;

   ----------
   -- W_AS --
   ----------

   procedure W_AS (Stream : not null access RST; Item : in Thin_Pointer) is
      T : constant S_AS := From_AS (Item);

   begin
      Ada.Streams.Write (Stream.all, T);
   end W_AS;

   ---------
   -- W_B --
   ---------

   procedure W_B (Stream : not null access RST; Item : in Boolean) is
      T : S_B;

   begin
      T (1) := Boolean'Pos (Item);
      Ada.Streams.Write (Stream.all, T);
   end W_B;

   ---------
   -- W_C --
   ---------

   procedure W_C (Stream : not null access RST; Item : in Character) is
      T : S_C;

   begin
      T (1) := Character'Pos (Item);
      Ada.Streams.Write (Stream.all, T);
   end W_C;

   ---------
   -- W_F --
   ---------

   procedure W_F (Stream : not null access RST; Item : in Float) is
      T : constant S_F := From_F (Item);

   begin
      Ada.Streams.Write (Stream.all, T);
   end W_F;

   ---------
   -- W_I --
   ---------

   procedure W_I (Stream : not null access RST; Item : in Integer) is
      T : constant S_I := From_I (Item);

   begin
      Ada.Streams.Write (Stream.all, T);
   end W_I;

   ----------
   -- W_LF --
   ----------

   procedure W_LF (Stream : not null access RST; Item : in Long_Float) is
      T : constant S_LF := From_LF (Item);

   begin
      Ada.Streams.Write (Stream.all, T);
   end W_LF;

   ----------
   -- W_LI --
   ----------

   procedure W_LI (Stream : not null access RST; Item : in Long_Integer) is
      T : constant S_LI := From_LI (Item);

   begin
      Ada.Streams.Write (Stream.all, T);
   end W_LI;

   -----------
   -- W_LLF --
   -----------

   procedure W_LLF (Stream : not null access RST; Item : in Long_Long_Float) is
      T : constant S_LLF := From_LLF (Item);

   begin
      Ada.Streams.Write (Stream.all, T);
   end W_LLF;

   -----------
   -- W_LLI --
   -----------

   procedure W_LLI
     (Stream : not null access RST; Item : in Long_Long_Integer)
   is
      T : constant S_LLI := From_LLI (Item);

   begin
      Ada.Streams.Write (Stream.all, T);
   end W_LLI;

   -----------
   -- W_LLU --
   -----------

   procedure W_LLU
     (Stream : not null access RST; Item : in UST.Long_Long_Unsigned)
   is
      T : constant S_LLU := From_LLU (Item);

   begin
      Ada.Streams.Write (Stream.all, T);
   end W_LLU;

   ----------
   -- W_LU --
   ----------

   procedure W_LU
     (Stream : not null access RST; Item : in UST.Long_Unsigned)
   is
      T : constant S_LU := From_LU (Item);

   begin
      Ada.Streams.Write (Stream.all, T);
   end W_LU;

   ----------
   -- W_SF --
   ----------

   procedure W_SF (Stream : not null access RST; Item : in Short_Float) is
      T : constant S_SF := From_SF (Item);

   begin
      Ada.Streams.Write (Stream.all, T);
   end W_SF;

   ----------
   -- W_SI --
   ----------

   procedure W_SI (Stream : not null access RST; Item : in Short_Integer) is
      T : constant S_SI := From_SI (Item);

   begin
      Ada.Streams.Write (Stream.all, T);
   end W_SI;

   -----------
   -- W_SSI --
   -----------

   procedure W_SSI
     (Stream : not null access RST; Item : in Short_Short_Integer)
   is
      T : constant S_SSI := From_SSI (Item);

   begin
      Ada.Streams.Write (Stream.all, T);
   end W_SSI;

   -----------
   -- W_SSU --
   -----------

   procedure W_SSU
     (Stream : not null access RST; Item : in UST.Short_Short_Unsigned)
   is
      T : constant S_SSU := From_SSU (Item);

   begin
      Ada.Streams.Write (Stream.all, T);
   end W_SSU;

   ----------
   -- W_SU --
   ----------

   procedure W_SU
     (Stream : not null access RST; Item : in UST.Short_Unsigned)
   is
      T : constant S_SU := From_SU (Item);

   begin
      Ada.Streams.Write (Stream.all, T);
   end W_SU;

   ---------
   -- W_U --
   ---------

   procedure W_U (Stream : not null access RST; Item : in UST.Unsigned) is
      T : constant S_U := From_U (Item);

   begin
      Ada.Streams.Write (Stream.all, T);
   end W_U;

   ----------
   -- W_WC --
   ----------

   procedure W_WC (Stream : not null access RST; Item : in Wide_Character) is
      T : constant S_WC := From_WC (Item);

   begin
      Ada.Streams.Write (Stream.all, T);
   end W_WC;

end System.Stream_Attributes;
