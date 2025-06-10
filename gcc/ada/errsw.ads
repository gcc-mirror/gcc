------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                E R R S W                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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

with Types; use Types;

package Errsw is

   type Status_Type is
     (Active,
      Deprecated);

   type Switch_Id is (
      No_Switch_Id,
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
      gnatytt,
      gnatel
   );

   subtype Active_Switch_Id is Switch_Id range gnatwb .. gnatel;

   type Switch_Type is record

      Status : Status_Type := Active;
      --  The status will indicate whether the switch is currently active,
      --  or has been deprecated. A deprecated switch will not control
      --  diagnostics, and will not be emitted by the GNAT usage.

      Human_Id : String_Ptr := null;
      --  The Human_Id will be a unique and stable string-based ID which
      --  identifies the content of the switch within the switch registry.
      --  This ID will appear in SARIF readers.

      Short_Name : String_Ptr := null;
      --  The Short_Name will denote the -gnatXX name of the switch.

      Description : String_Ptr := null;
      --  The description will contain the description of the switch, as it is
      --  currently emitted by the GNAT usage.

      Documentation_Url : String_Ptr := null;
      --  The documentation_url will point to the AdaCore documentation site
      --  for the switch.

   end record;

   function Get_Switch (Id : Switch_Id) return Switch_Type;

   function Get_Switch_Id (Name : String) return Switch_Id;

   procedure Print_Switch_Repository;

end Errsw;
