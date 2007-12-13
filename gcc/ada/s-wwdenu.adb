------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . W W D _ E N U M                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2007, Free Software Foundation, Inc.         --
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

with System.WCh_StW; use System.WCh_StW;
with System.WCh_Con; use System.WCh_Con;

with Ada.Unchecked_Conversion;

package body System.WWd_Enum is

   -----------------------------------
   -- Wide_Wide_Width_Enumeration_8 --
   -----------------------------------

   function Wide_Wide_Width_Enumeration_8
     (Names   : String;
      Indexes : System.Address;
      Lo, Hi  : Natural;
      EM      : WC_Encoding_Method) return Natural
   is
      W : Natural;

      type Natural_8 is range 0 .. 2 ** 7 - 1;
      type Index_Table is array (Natural) of Natural_8;
      type Index_Table_Ptr is access Index_Table;

      function To_Index_Table_Ptr is
        new Ada.Unchecked_Conversion (System.Address, Index_Table_Ptr);

      IndexesT : constant Index_Table_Ptr := To_Index_Table_Ptr (Indexes);

   begin
      W := 0;
      for J in Lo .. Hi loop
         declare
            S  : constant String :=
                   Names (Natural (IndexesT (J)) ..
                          Natural (IndexesT (J + 1)) - 1);
            WS : Wide_Wide_String (1 .. S'Length);
            L  : Natural;
         begin
            String_To_Wide_Wide_String (S, WS, L, EM);
            W := Natural'Max (W, L);
         end;
      end loop;

      return W;
   end Wide_Wide_Width_Enumeration_8;

   ------------------------------------
   -- Wide_Wide_Width_Enumeration_16 --
   ------------------------------------

   function Wide_Wide_Width_Enumeration_16
     (Names   : String;
      Indexes : System.Address;
      Lo, Hi  : Natural;
      EM      : WC_Encoding_Method) return Natural
   is
      W : Natural;

      type Natural_16 is range 0 .. 2 ** 15 - 1;
      type Index_Table is array (Natural) of Natural_16;
      type Index_Table_Ptr is access Index_Table;

      function To_Index_Table_Ptr is
        new Ada.Unchecked_Conversion (System.Address, Index_Table_Ptr);

      IndexesT : constant Index_Table_Ptr := To_Index_Table_Ptr (Indexes);

   begin
      W := 0;
      for J in Lo .. Hi loop
         declare
            S  : constant String :=
                   Names (Natural (IndexesT (J)) ..
                          Natural (IndexesT (J + 1)) - 1);
            WS : Wide_Wide_String (1 .. S'Length);
            L  : Natural;
         begin
            String_To_Wide_Wide_String (S, WS, L, EM);
            W := Natural'Max (W, L);
         end;
      end loop;

      return W;
   end Wide_Wide_Width_Enumeration_16;

   ------------------------------------
   -- Wide_Wide_Width_Enumeration_32 --
   ------------------------------------

   function Wide_Wide_Width_Enumeration_32
     (Names   : String;
      Indexes : System.Address;
      Lo, Hi  : Natural;
      EM      : WC_Encoding_Method) return Natural
   is
      W : Natural;

      type Natural_32 is range 0 .. 2 ** 31 - 1;
      type Index_Table is array (Natural) of Natural_32;
      type Index_Table_Ptr is access Index_Table;

      function To_Index_Table_Ptr is
        new Ada.Unchecked_Conversion (System.Address, Index_Table_Ptr);

      IndexesT : constant Index_Table_Ptr := To_Index_Table_Ptr (Indexes);

   begin
      W := 0;
      for J in Lo .. Hi loop
         declare
            S  : constant String :=
                   Names (Natural (IndexesT (J)) ..
                          Natural (IndexesT (J + 1)) - 1);
            WS : Wide_Wide_String (1 .. S'Length);
            L  : Natural;
         begin
            String_To_Wide_Wide_String (S, WS, L, EM);
            W := Natural'Max (W, L);
         end;
      end loop;

      return W;
   end Wide_Wide_Width_Enumeration_32;

   ------------------------------
   -- Wide_Width_Enumeration_8 --
   ------------------------------

   function Wide_Width_Enumeration_8
     (Names   : String;
      Indexes : System.Address;
      Lo, Hi  : Natural;
      EM      : WC_Encoding_Method) return Natural
   is
      W : Natural;

      type Natural_8 is range 0 .. 2 ** 7 - 1;
      type Index_Table is array (Natural) of Natural_8;
      type Index_Table_Ptr is access Index_Table;

      function To_Index_Table_Ptr is
        new Ada.Unchecked_Conversion (System.Address, Index_Table_Ptr);

      IndexesT : constant Index_Table_Ptr := To_Index_Table_Ptr (Indexes);

   begin
      W := 0;
      for J in Lo .. Hi loop
         declare
            S  : constant String :=
                   Names (Natural (IndexesT (J)) ..
                          Natural (IndexesT (J + 1)) - 1);
            WS : Wide_String (1 .. S'Length);
            L  : Natural;
         begin
            String_To_Wide_String (S, WS, L, EM);
            W := Natural'Max (W, L);
         end;
      end loop;

      return W;
   end Wide_Width_Enumeration_8;

   -------------------------------
   -- Wide_Width_Enumeration_16 --
   -------------------------------

   function Wide_Width_Enumeration_16
     (Names   : String;
      Indexes : System.Address;
      Lo, Hi  : Natural;
      EM      : WC_Encoding_Method) return Natural
   is
      W : Natural;

      type Natural_16 is range 0 .. 2 ** 15 - 1;
      type Index_Table is array (Natural) of Natural_16;
      type Index_Table_Ptr is access Index_Table;

      function To_Index_Table_Ptr is
        new Ada.Unchecked_Conversion (System.Address, Index_Table_Ptr);

      IndexesT : constant Index_Table_Ptr := To_Index_Table_Ptr (Indexes);

   begin
      W := 0;
      for J in Lo .. Hi loop
         declare
            S  : constant String :=
                   Names (Natural (IndexesT (J)) ..
                          Natural (IndexesT (J + 1)) - 1);
            WS : Wide_String (1 .. S'Length);
            L  : Natural;
         begin
            String_To_Wide_String (S, WS, L, EM);
            W := Natural'Max (W, L);
         end;
      end loop;

      return W;
   end Wide_Width_Enumeration_16;

   -------------------------------
   -- Wide_Width_Enumeration_32 --
   -------------------------------

   function Wide_Width_Enumeration_32
     (Names   : String;
      Indexes : System.Address;
      Lo, Hi  : Natural;
      EM      : WC_Encoding_Method) return Natural
   is
      W : Natural;

      type Natural_32 is range 0 .. 2 ** 31 - 1;
      type Index_Table is array (Natural) of Natural_32;
      type Index_Table_Ptr is access Index_Table;

      function To_Index_Table_Ptr is
        new Ada.Unchecked_Conversion (System.Address, Index_Table_Ptr);

      IndexesT : constant Index_Table_Ptr := To_Index_Table_Ptr (Indexes);

   begin
      W := 0;
      for J in Lo .. Hi loop
         declare
            S  : constant String :=
                   Names (Natural (IndexesT (J)) ..
                          Natural (IndexesT (J + 1)) - 1);
            WS : Wide_String (1 .. S'Length);
            L  : Natural;
         begin
            String_To_Wide_String (S, WS, L, EM);
            W := Natural'Max (W, L);
         end;
      end loop;

      return W;
   end Wide_Width_Enumeration_32;

end System.WWd_Enum;
