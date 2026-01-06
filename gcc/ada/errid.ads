------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                E R R I D                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2026, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

package Errid is

   type Status_Type is
     (Active,
      Deprecated);

   type Diagnostic_Id is
     (No_Diagnostic_Id,
      GNAT0001,
      GNAT0002,
      GNAT0003,
      GNAT0004,
      GNAT0005,
      GNAT0006,
      GNAT0007,
      GNAT0008,
      GNAT0009);

   type Diagnostic_Id_Array is array (Positive range <>) of Diagnostic_Id;
   type Diagnostic_Id_Array_Access is access Diagnostic_Id_Array;
   type Switch_Id is (
      No_Switch_Id,
      gnatel,
      gnatwb,
      gnatwc,
      gnatwd,
      gnatwf,
      gnatwg,
      gnatwh,
      gnatwi,
      gnatwj,
      gnatwk,
      gnatwl,
      gnatwm,
      gnatwo,
      gnatwp,
      gnatwq,
      gnatwr,
      gnatwt,
      gnatwu,
      gnatwv,
      gnatww,
      gnatwx,
      gnatwy,
      gnatwz,
      gnatw_dot_a,
      gnatw_dot_b,
      gnatw_dot_c,
      gnatw_dot_f,
      gnatw_dot_h,
      gnatw_dot_i,
      gnatw_dot_j,
      gnatw_dot_k,
      gnatw_dot_l,
      gnatw_dot_m,
      gnatw_dot_n,
      gnatw_dot_o,
      gnatw_dot_p,
      gnatw_dot_q,
      gnatw_dot_r,
      gnatw_dot_s,
      gnatw_dot_t,
      gnatw_dot_u,
      gnatw_dot_v,
      gnatw_dot_w,
      gnatw_dot_x,
      gnatw_dot_y,
      gnatw_dot_z,
      gnatw_underscore_a,
      gnatw_underscore_c,
      gnatw_underscore_j,
      gnatw_underscore_l,
      gnatw_underscore_p,
      gnatw_underscore_q,
      gnatw_underscore_r,
      gnatw_underscore_s,
      gnaty,
      gnatya,
      gnatyb,
      gnatyc,
      gnatyd,
      gnatye,
      gnatyf,
      gnatyh,
      gnatyi,
      gnatyk,
      gnatyl,
      gnatym,
      gnatyn,
      gnatyo,
      gnatyp,
      gnatyr,
      gnatys,
      gnatyu,
      gnatyx,
      gnatyz,
      gnatyaa,
      gnatybb,
      gnatycc,
      gnatydd,
      gnatyii,
      gnatyll,
      gnatymm,
      gnatyoo,
      gnatyss,
      gnatytt
   );

   subtype Active_Switch_Id is Switch_Id range gnatel .. gnatytt;

   function Get_Documentation_File (Id : Diagnostic_Id) return String;
   --  Return the location of the documentation file as a string.

   function To_String (Id : Diagnostic_Id) return String;
   --  Convert the diagnostic ID to a 4 character string padded with 0-s.

   procedure Print_Diagnostic_Repository;
   --  Print all of the Diagnostic_Id-s and Switch_Id-s as rules in the SARIF
   --  format.

end Errid;
