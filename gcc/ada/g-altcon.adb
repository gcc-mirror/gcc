------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--             G N A T . A L T I V E C . C O N V E R S I O N S              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2005, Free Software Foundation, Inc.            --
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

with Ada.Unchecked_Conversion;

with System; use System;

with GNAT.Altivec.Low_Level_Interface; use GNAT.Altivec.Low_Level_Interface;
with GNAT.Altivec.Low_Level_Vectors;   use GNAT.Altivec.Low_Level_Vectors;

package body GNAT.Altivec.Conversions is

   function To_Varray_unsigned_char is
     new Ada.Unchecked_Conversion (Varray_signed_char,
                                   Varray_unsigned_char);

   function To_Varray_unsigned_char is
     new Ada.Unchecked_Conversion (Varray_bool_char,
                                   Varray_unsigned_char);

   function To_Varray_unsigned_short is
     new Ada.Unchecked_Conversion (Varray_signed_short,
                                   Varray_unsigned_short);

   function To_Varray_unsigned_short is
     new Ada.Unchecked_Conversion (Varray_bool_short,
                                   Varray_unsigned_short);

   function To_Varray_unsigned_short is
      new Ada.Unchecked_Conversion (Varray_pixel,
                                    Varray_unsigned_short);

   function To_Varray_unsigned_int is
     new Ada.Unchecked_Conversion (Varray_signed_int,
                                   Varray_unsigned_int);

   function To_Varray_unsigned_int is
     new Ada.Unchecked_Conversion (Varray_bool_int,
                                   Varray_unsigned_int);

   function To_Varray_unsigned_int is
      new Ada.Unchecked_Conversion (Varray_float,
                                    Varray_unsigned_int);

   function To_Varray_signed_char is
     new Ada.Unchecked_Conversion (Varray_unsigned_char,
                                   Varray_signed_char);

   function To_Varray_bool_char is
     new Ada.Unchecked_Conversion (Varray_unsigned_char,
                                   Varray_bool_char);

   function To_Varray_signed_short is
     new Ada.Unchecked_Conversion (Varray_unsigned_short,
                                   Varray_signed_short);

   function To_Varray_bool_short is
     new Ada.Unchecked_Conversion (Varray_unsigned_short,
                                   Varray_bool_short);

   function To_Varray_pixel is
     new Ada.Unchecked_Conversion (Varray_unsigned_short,
                                   Varray_pixel);

   function To_Varray_signed_int is
     new Ada.Unchecked_Conversion (Varray_unsigned_int,
                                   Varray_signed_int);

   function To_Varray_bool_int is
     new Ada.Unchecked_Conversion (Varray_unsigned_int,
                                   Varray_bool_int);

   function To_Varray_float is
     new Ada.Unchecked_Conversion (Varray_unsigned_int,
                                   Varray_float);

   function To_VUC is new Ada.Unchecked_Conversion (VUC_View, VUC);
   function To_VSC is new Ada.Unchecked_Conversion (VSC_View, VSC);
   function To_VBC is new Ada.Unchecked_Conversion (VBC_View, VBC);
   function To_VUS is new Ada.Unchecked_Conversion (VUS_View, VUS);
   function To_VSS is new Ada.Unchecked_Conversion (VSS_View, VSS);
   function To_VBS is new Ada.Unchecked_Conversion (VBS_View, VBS);
   function To_VUI is new Ada.Unchecked_Conversion (VUI_View, VUI);
   function To_VSI is new Ada.Unchecked_Conversion (VSI_View, VSI);
   function To_VBI is new Ada.Unchecked_Conversion (VBI_View, VBI);
   function To_VF  is new Ada.Unchecked_Conversion (VF_View,  VF);
   function To_VP  is new Ada.Unchecked_Conversion (VP_View,  VP);

   function To_VUC_View is new Ada.Unchecked_Conversion (VUC, VUC_View);
   function To_VSC_View is new Ada.Unchecked_Conversion (VSC, VSC_View);
   function To_VBC_View is new Ada.Unchecked_Conversion (VBC, VBC_View);
   function To_VUS_View is new Ada.Unchecked_Conversion (VUS, VUS_View);
   function To_VSS_View is new Ada.Unchecked_Conversion (VSS, VSS_View);
   function To_VBS_View is new Ada.Unchecked_Conversion (VBS, VBS_View);
   function To_VUI_View is new Ada.Unchecked_Conversion (VUI, VUI_View);
   function To_VSI_View is new Ada.Unchecked_Conversion (VSI, VSI_View);
   function To_VBI_View is new Ada.Unchecked_Conversion (VBI, VBI_View);
   function To_VF_View  is new Ada.Unchecked_Conversion (VF,  VF_View);
   function To_VP_View  is new Ada.Unchecked_Conversion (VP,  VP_View);

   pragma Warnings (Off, Default_Bit_Order);

   ---------------
   -- To_Vector --
   ---------------

   function To_Vector (S : VSC_View) return VSC is
   begin
      if Default_Bit_Order = High_Order_First then
         return To_VSC (S);
      else
         declare
            Result : LL_VUC;
            VS     : constant VUC_View :=
                       (Values => To_Varray_unsigned_char (S.Values));
         begin
            Result := To_Vector (VS);
            return To_LL_VSC (Result);
         end;
      end if;
   end To_Vector;

   function To_Vector (S : VBC_View) return VBC is
   begin
      if Default_Bit_Order = High_Order_First then
         return To_VBC (S);
      else
         declare
            Result : LL_VUC;
            VS     : constant VUC_View :=
                       (Values => To_Varray_unsigned_char (S.Values));
         begin
            Result := To_Vector (VS);
            return To_LL_VBC (Result);
         end;
      end if;
   end To_Vector;

   function To_Vector (S : VSS_View) return VSS is
   begin
      if Default_Bit_Order = High_Order_First then
         return To_VSS (S);
      else
         declare
            Result : LL_VUS;
            VS     : constant VUS_View :=
                       (Values => To_Varray_unsigned_short (S.Values));
         begin
            Result := To_Vector (VS);
            return VSS (To_LL_VSS (Result));
         end;
      end if;
   end To_Vector;

   function To_Vector (S : VBS_View) return VBS is
   begin
      if Default_Bit_Order = High_Order_First then
         return To_VBS (S);
      else
         declare
            Result : LL_VUS;
            VS     : constant VUS_View :=
                       (Values => To_Varray_unsigned_short (S.Values));
         begin
            Result := To_Vector (VS);
            return To_LL_VBS (Result);
         end;
      end if;
   end To_Vector;

   function To_Vector (S : VP_View) return VP is
   begin
      if Default_Bit_Order = High_Order_First then
         return To_VP (S);
      else
         declare
            Result : LL_VUS;
            VS     : constant VUS_View :=
                       (Values => To_Varray_unsigned_short (S.Values));
         begin
            Result := To_Vector (VS);
            return To_LL_VP (Result);
         end;
      end if;
   end To_Vector;

   function To_Vector (S : VSI_View) return VSI is
   begin
      if Default_Bit_Order = High_Order_First then
         return To_VSI (S);
      else
         declare
            Result : LL_VUI;
            VS     : constant VUI_View :=
                       (Values => To_Varray_unsigned_int (S.Values));
         begin
            Result := To_Vector (VS);
            return To_LL_VSI (Result);
         end;
      end if;
   end To_Vector;

   function To_Vector (S : VBI_View) return VBI is
   begin
      if Default_Bit_Order = High_Order_First then
         return To_VBI (S);
      else
         declare
            Result : LL_VUI;
            VS     : constant VUI_View :=
                       (Values => To_Varray_unsigned_int (S.Values));
         begin
            Result := To_Vector (VS);
            return To_LL_VBI (Result);
         end;
      end if;
   end To_Vector;

   function To_Vector (S : VF_View) return VF is
   begin
      if Default_Bit_Order = High_Order_First then
         return To_VF (S);
      else
         declare
            Result : LL_VUI;
            VS     : constant VUI_View :=
                       (Values => To_Varray_unsigned_int (S.Values));
         begin
            Result := To_Vector (VS);
            return To_LL_VF (Result);
         end;
      end if;
   end To_Vector;

   function To_Vector (S : VUC_View) return VUC is
   begin
      if Default_Bit_Order = High_Order_First then
         return To_VUC (S);
      else
         declare
            Result : VUC_View;
         begin
            for J in Vchar_Range'Range loop
               Result.Values (J) :=
                 S.Values (Vchar_Range'Last - J + Vchar_Range'First);
            end loop;
            return To_VUC (Result);
         end;
      end if;
   end To_Vector;

   function To_Vector (S : VUS_View) return VUS is
   begin
      if Default_Bit_Order = High_Order_First then
         return To_VUS (S);
      else
         declare
            Result : VUS_View;
         begin
            for J in Vshort_Range'Range loop
               Result.Values (J) :=
                 S.Values (Vshort_Range'Last - J + Vshort_Range'First);
            end loop;
            return To_VUS (Result);
         end;
      end if;
   end To_Vector;

   function To_Vector (S : VUI_View) return VUI is
   begin
      if Default_Bit_Order = High_Order_First then
         return To_VUI (S);
      else
         declare
            Result : VUI_View;
         begin
            for J in Vint_Range'Range loop
               Result.Values (J) :=
                 S.Values (Vint_Range'Last - J + Vint_Range'First);
            end loop;
            return To_VUI (Result);
         end;
      end if;
   end To_Vector;

   --------------
   -- To_View --
   --------------

   function To_View (S : VSC) return VSC_View is
   begin
      if Default_Bit_Order = High_Order_First then
         return To_VSC_View (S);
      else
         declare
            Result : VUC_View;
         begin
            Result := To_View (To_LL_VUC (S));
            return (Values => To_Varray_signed_char (Result.Values));
         end;
      end if;
   end To_View;

   function To_View (S : VBC) return VBC_View is
   begin
      if Default_Bit_Order = High_Order_First then
         return To_VBC_View (S);
      else
         declare
            Result : VUC_View;
         begin
            Result := To_View (To_LL_VUC (S));
            return (Values => To_Varray_bool_char (Result.Values));
         end;
      end if;
   end To_View;

   function To_View (S : VSS) return VSS_View is
   begin
      if Default_Bit_Order = High_Order_First then
         return To_VSS_View (S);
      else
         declare
            Result : VUS_View;
         begin
            Result := To_View (To_LL_VUS (S));
            return (Values => To_Varray_signed_short (Result.Values));
         end;
      end if;
   end To_View;

   function To_View (S : VBS) return VBS_View is
   begin
      if Default_Bit_Order = High_Order_First then
         return To_VBS_View (S);
      else
         declare
            Result : VUS_View;
         begin
            Result := To_View (To_LL_VUS (S));
            return (Values => To_Varray_bool_short (Result.Values));
         end;
      end if;
   end To_View;

   function To_View (S : VP) return VP_View is
   begin
      if Default_Bit_Order = High_Order_First then
         return To_VP_View (S);
      else
         declare
            Result : VUS_View;
         begin
            Result := To_View (To_LL_VUS (S));
            return (Values => To_Varray_pixel (Result.Values));
         end;
      end if;
   end To_View;

   function To_View (S : VSI) return VSI_View is
   begin
      if Default_Bit_Order = High_Order_First then
         return To_VSI_View (S);
      else
         declare
            Result : VUI_View;
         begin
            Result := To_View (To_LL_VUI (S));
            return (Values => To_Varray_signed_int (Result.Values));
         end;
      end if;
   end To_View;

   function To_View (S : VBI) return VBI_View is
   begin
      if Default_Bit_Order = High_Order_First then
         return To_VBI_View (S);
      else
         declare
            Result : VUI_View;
         begin
            Result := To_View (To_LL_VUI (S));
            return (Values => To_Varray_bool_int (Result.Values));
         end;
      end if;
   end To_View;

   function To_View (S : VF) return VF_View is
   begin
      if Default_Bit_Order = High_Order_First then
         return To_VF_View (S);
      else
         declare
            Result : VUI_View;
         begin
            Result := To_View (To_LL_VUI (S));
            return (Values => To_Varray_float (Result.Values));
         end;
      end if;
   end To_View;

   function To_View (S : VUC) return VUC_View is
   begin
      if Default_Bit_Order = High_Order_First then
         return To_VUC_View (S);
      else
         declare
            VS     : constant VUC_View := To_VUC_View (S);
            Result : VUC_View;
         begin
            for J in Vchar_Range'Range loop
               Result.Values (J) :=
                 VS.Values (Vchar_Range'Last - J + Vchar_Range'First);
            end loop;
            return Result;
         end;
      end if;
   end To_View;

   function To_View (S : VUS) return VUS_View is
   begin
      if Default_Bit_Order = High_Order_First then
         return To_VUS_View (S);
      else
         declare
            VS     : constant VUS_View := To_VUS_View (S);
            Result : VUS_View;
         begin
            for J in Vshort_Range'Range loop
               Result.Values (J) :=
                 VS.Values (Vshort_Range'Last - J + Vshort_Range'First);
            end loop;
            return Result;
         end;
      end if;
   end To_View;

   function To_View (S : VUI) return VUI_View is
   begin
      if Default_Bit_Order = High_Order_First then
         return To_VUI_View (S);
      else
         declare
            VS     : constant VUI_View := To_VUI_View (S);
            Result : VUI_View;
         begin
            for J in Vint_Range'Range loop
               Result.Values (J) :=
                 VS.Values (Vint_Range'Last - J + Vint_Range'First);
            end loop;
            return Result;
         end;
      end if;
   end To_View;

end GNAT.Altivec.Conversions;
