------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--             G N A T . A L T I V E C . C O N V E R S I O N S              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2009, Free Software Foundation, Inc.            --
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

--  This unit provides the Vector/Views conversions

with GNAT.Altivec.Vector_Types; use GNAT.Altivec.Vector_Types;
with GNAT.Altivec.Vector_Views; use GNAT.Altivec.Vector_Views;

package GNAT.Altivec.Conversions is

   ---------------------
   -- char components --
   ---------------------

   function To_Vector (S : VUC_View) return VUC;
   function To_Vector (S : VSC_View) return VSC;
   function To_Vector (S : VBC_View) return VBC;

   function To_View (S : VUC) return VUC_View;
   function To_View (S : VSC) return VSC_View;
   function To_View (S : VBC) return VBC_View;

   ----------------------
   -- short components --
   ----------------------

   function To_Vector (S : VUS_View) return VUS;
   function To_Vector (S : VSS_View) return VSS;
   function To_Vector (S : VBS_View) return VBS;

   function To_View (S : VUS) return VUS_View;
   function To_View (S : VSS) return VSS_View;
   function To_View (S : VBS) return VBS_View;

   --------------------
   -- int components --
   --------------------

   function To_Vector (S : VUI_View) return VUI;
   function To_Vector (S : VSI_View) return VSI;
   function To_Vector (S : VBI_View) return VBI;

   function To_View (S : VUI) return VUI_View;
   function To_View (S : VSI) return VSI_View;
   function To_View (S : VBI) return VBI_View;

   ----------------------
   -- float components --
   ----------------------

   function To_Vector (S : VF_View) return VF;

   function To_View (S : VF) return VF_View;

   ----------------------
   -- pixel components --
   ----------------------

   function To_Vector (S : VP_View) return VP;

   function To_View (S : VP) return VP_View;

private

   --  We want the above subprograms to always be inlined in the case of the
   --  hard PowerPC AltiVec support in order to avoid the unnecessary function
   --  call. On the other hand there is no problem with inlining these
   --  subprograms on little-endian targets.

   pragma Inline_Always (To_Vector);
   pragma Inline_Always (To_View);

end GNAT.Altivec.Conversions;
