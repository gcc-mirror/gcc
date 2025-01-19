------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--       G N A T . A L T I V E C . L O W _ L E V E L _ V E C T O R S        --
--                                                                          --
--                                 S p e c                                  --
--                         (Soft Binding Version)                           --
--                                                                          --
--          Copyright (C) 2004-2025, Free Software Foundation, Inc.         --
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

--  This unit exposes the low level vector support for the Soft binding,
--  intended for non AltiVec capable targets. See Altivec.Design for a
--  description of what is expected to be exposed.

with GNAT.Altivec.Vector_Views; use GNAT.Altivec.Vector_Views;

package GNAT.Altivec.Low_Level_Vectors is

   ----------------------------------------
   -- Low level vector type declarations --
   ----------------------------------------

   type LL_VUC is private;
   type LL_VSC is private;
   type LL_VBC is private;

   type LL_VUS is private;
   type LL_VSS is private;
   type LL_VBS is private;

   type LL_VUI is private;
   type LL_VSI is private;
   type LL_VBI is private;

   type LL_VF  is private;
   type LL_VP  is private;

   ------------------------------------
   -- Low level functional interface --
   ------------------------------------

   function abs_v16qi (A : LL_VSC) return LL_VSC;
   function abs_v8hi  (A : LL_VSS) return LL_VSS;
   function abs_v4si  (A : LL_VSI) return LL_VSI;
   function abs_v4sf  (A : LL_VF)  return LL_VF;

   function abss_v16qi (A : LL_VSC) return LL_VSC;
   function abss_v8hi  (A : LL_VSS) return LL_VSS;
   function abss_v4si  (A : LL_VSI) return LL_VSI;

   function vaddubm (A : LL_VSC; B : LL_VSC) return LL_VSC;
   function vadduhm (A : LL_VSS; B : LL_VSS) return LL_VSS;
   function vadduwm (A : LL_VSI; B : LL_VSI) return LL_VSI;
   function vaddfp  (A : LL_VF;  B : LL_VF)  return LL_VF;

   function vaddcuw (A : LL_VSI; B : LL_VSI) return LL_VSI;

   function vaddubs (A : LL_VSC; B : LL_VSC) return LL_VSC;
   function vaddsbs (A : LL_VSC; B : LL_VSC) return LL_VSC;
   function vadduhs (A : LL_VSS; B : LL_VSS) return LL_VSS;
   function vaddshs (A : LL_VSS; B : LL_VSS) return LL_VSS;
   function vadduws (A : LL_VSI; B : LL_VSI) return LL_VSI;
   function vaddsws (A : LL_VSI; B : LL_VSI) return LL_VSI;

   function vand  (A : LL_VSI; B : LL_VSI) return LL_VSI;
   function vandc (A : LL_VSI; B : LL_VSI) return LL_VSI;

   function vavgub (A : LL_VSC; B : LL_VSC) return LL_VSC;
   function vavgsb (A : LL_VSC; B : LL_VSC) return LL_VSC;
   function vavguh (A : LL_VSS; B : LL_VSS) return LL_VSS;
   function vavgsh (A : LL_VSS; B : LL_VSS) return LL_VSS;
   function vavguw (A : LL_VSI; B : LL_VSI) return LL_VSI;
   function vavgsw (A : LL_VSI; B : LL_VSI) return LL_VSI;

   function vcmpbfp (A : LL_VF; B : LL_VF) return LL_VSI;

   function vcmpequb (A : LL_VSC; B : LL_VSC) return LL_VSC;
   function vcmpequh (A : LL_VSS; B : LL_VSS) return LL_VSS;
   function vcmpequw (A : LL_VSI; B : LL_VSI) return LL_VSI;
   function vcmpeqfp (A : LL_VF; B : LL_VF) return LL_VSI;

   function vcmpgefp (A : LL_VF; B : LL_VF) return LL_VSI;

   function vcmpgtub (A : LL_VSC; B : LL_VSC) return LL_VSC;
   function vcmpgtsb (A : LL_VSC; B : LL_VSC) return LL_VSC;
   function vcmpgtuh (A : LL_VSS; B : LL_VSS) return LL_VSS;
   function vcmpgtsh (A : LL_VSS; B : LL_VSS) return LL_VSS;
   function vcmpgtuw (A : LL_VSI; B : LL_VSI) return LL_VSI;
   function vcmpgtsw (A : LL_VSI; B : LL_VSI) return LL_VSI;
   function vcmpgtfp (A : LL_VF; B : LL_VF) return LL_VSI;

   function vcfux (A : LL_VUI; B : c_int) return LL_VF;
   function vcfsx (A : LL_VSI; B : c_int) return LL_VF;

   function vctsxs (A : LL_VF; B : c_int) return LL_VSI;
   function vctuxs (A : LL_VF; B : c_int) return LL_VUI;

   procedure dss (A : c_int);
   procedure dssall;

   procedure dst    (A : c_ptr; B : c_int; C : c_int);
   procedure dstst  (A : c_ptr; B : c_int; C : c_int);
   procedure dststt (A : c_ptr; B : c_int; C : c_int);
   procedure dstt   (A : c_ptr; B : c_int; C : c_int);

   function vexptefp (A : LL_VF) return LL_VF;

   function vrfim (A : LL_VF) return LL_VF;

   function lvx   (A : c_long; B : c_ptr) return LL_VSI;
   function lvebx (A : c_long; B : c_ptr) return LL_VSC;
   function lvehx (A : c_long; B : c_ptr) return LL_VSS;
   function lvewx (A : c_long; B : c_ptr) return LL_VSI;
   function lvxl  (A : c_long; B : c_ptr) return LL_VSI;

   function vlogefp (A : LL_VF) return LL_VF;

   function lvsl  (A : c_long; B : c_ptr) return LL_VSC;
   function lvsr  (A : c_long; B : c_ptr) return LL_VSC;

   function vmaddfp (A : LL_VF; B : LL_VF; C : LL_VF) return LL_VF;

   function vmhaddshs  (A : LL_VSS; B : LL_VSS; C : LL_VSS) return LL_VSS;

   function vmaxub (A : LL_VSC; B : LL_VSC) return LL_VSC;
   function vmaxsb (A : LL_VSC; B : LL_VSC) return LL_VSC;
   function vmaxuh (A : LL_VSS; B : LL_VSS) return LL_VSS;
   function vmaxsh (A : LL_VSS; B : LL_VSS) return LL_VSS;
   function vmaxuw (A : LL_VSI; B : LL_VSI) return LL_VSI;
   function vmaxsw (A : LL_VSI; B : LL_VSI) return LL_VSI;
   function vmaxfp (A : LL_VF;  B : LL_VF)  return LL_VF;

   function vmrghb (A : LL_VSC; B : LL_VSC) return LL_VSC;
   function vmrghh (A : LL_VSS; B : LL_VSS) return LL_VSS;
   function vmrghw (A : LL_VSI; B : LL_VSI) return LL_VSI;
   function vmrglb (A : LL_VSC; B : LL_VSC) return LL_VSC;
   function vmrglh (A : LL_VSS; B : LL_VSS) return LL_VSS;
   function vmrglw (A : LL_VSI; B : LL_VSI) return LL_VSI;

   function mfvscr return LL_VSS;

   function vminfp (A : LL_VF;  B : LL_VF)  return LL_VF;
   function vminsb (A : LL_VSC; B : LL_VSC) return LL_VSC;
   function vminsh (A : LL_VSS; B : LL_VSS) return LL_VSS;
   function vminsw (A : LL_VSI; B : LL_VSI) return LL_VSI;
   function vminub (A : LL_VSC; B : LL_VSC) return LL_VSC;
   function vminuh (A : LL_VSS; B : LL_VSS) return LL_VSS;
   function vminuw (A : LL_VSI; B : LL_VSI) return LL_VSI;

   function vmladduhm (A : LL_VSS; B : LL_VSS; C : LL_VSS) return LL_VSS;

   function vmhraddshs (A : LL_VSS; B : LL_VSS; C : LL_VSS) return LL_VSS;

   function vmsumubm (A : LL_VSC; B : LL_VSC; C : LL_VSI) return LL_VSI;
   function vmsummbm (A : LL_VSC; B : LL_VSC; C : LL_VSI) return LL_VSI;
   function vmsumuhm (A : LL_VSS; B : LL_VSS; C : LL_VSI) return LL_VSI;
   function vmsumshm (A : LL_VSS; B : LL_VSS; C : LL_VSI) return LL_VSI;
   function vmsumuhs (A : LL_VSS; B : LL_VSS; C : LL_VSI) return LL_VSI;
   function vmsumshs (A : LL_VSS; B : LL_VSS; C : LL_VSI) return LL_VSI;

   procedure mtvscr (A : LL_VSI);

   function vmuleub (A : LL_VSC; B : LL_VSC) return LL_VSS;
   function vmuleuh (A : LL_VSS; B : LL_VSS) return LL_VSI;
   function vmulesb (A : LL_VSC; B : LL_VSC) return LL_VSS;
   function vmulesh (A : LL_VSS; B : LL_VSS) return LL_VSI;

   function vmulosb (A : LL_VSC; B : LL_VSC) return LL_VSS;
   function vmulosh (A : LL_VSS; B : LL_VSS) return LL_VSI;
   function vmuloub (A : LL_VSC; B : LL_VSC) return LL_VSS;
   function vmulouh (A : LL_VSS; B : LL_VSS) return LL_VSI;

   function vnmsubfp (A : LL_VF; B : LL_VF; C : LL_VF) return LL_VF;

   function vxor (A : LL_VSI; B : LL_VSI) return LL_VSI;
   function vnor (A : LL_VSI; B : LL_VSI) return LL_VSI;
   function vor  (A : LL_VSI; B : LL_VSI) return LL_VSI;

   function vpkuhum (A : LL_VSS; B : LL_VSS) return LL_VSC;
   function vpkuwum (A : LL_VSI; B : LL_VSI) return LL_VSS;
   function vpkpx   (A : LL_VSI; B : LL_VSI) return LL_VSS;
   function vpkuhus (A : LL_VSS; B : LL_VSS) return LL_VSC;
   function vpkuwus (A : LL_VSI; B : LL_VSI) return LL_VSS;
   function vpkshss (A : LL_VSS; B : LL_VSS) return LL_VSC;
   function vpkswss (A : LL_VSI; B : LL_VSI) return LL_VSS;
   function vpkshus (A : LL_VSS; B : LL_VSS) return LL_VSC;
   function vpkswus (A : LL_VSI; B : LL_VSI) return LL_VSS;

   function vperm_4si (A : LL_VSI; B : LL_VSI; C : LL_VSC) return LL_VSI;

   function vrefp (A : LL_VF) return LL_VF;

   function vrlb (A : LL_VSC; B : LL_VSC) return LL_VSC;
   function vrlh (A : LL_VSS; B : LL_VSS) return LL_VSS;
   function vrlw (A : LL_VSI; B : LL_VSI) return LL_VSI;

   function vrfin (A : LL_VF) return LL_VF;
   function vrfip (A : LL_VF) return LL_VF;
   function vrfiz (A : LL_VF) return LL_VF;

   function vrsqrtefp (A : LL_VF) return LL_VF;

   function vsel_4si (A : LL_VSI; B : LL_VSI; C : LL_VSI) return LL_VSI;

   function vslb (A : LL_VSC; B : LL_VSC) return LL_VSC;
   function vslh (A : LL_VSS; B : LL_VSS) return LL_VSS;
   function vslw (A : LL_VSI; B : LL_VSI) return LL_VSI;

   function vsldoi_4si  (A : LL_VSI; B : LL_VSI; C : c_int) return LL_VSI;
   function vsldoi_8hi  (A : LL_VSS; B : LL_VSS; C : c_int) return LL_VSS;
   function vsldoi_16qi (A : LL_VSC; B : LL_VSC; C : c_int) return LL_VSC;
   function vsldoi_4sf  (A : LL_VF;  B : LL_VF;  C : c_int) return LL_VF;

   function vsl  (A : LL_VSI; B : LL_VSI) return LL_VSI;
   function vslo (A : LL_VSI; B : LL_VSI) return LL_VSI;

   function vspltb (A : LL_VSC; B : c_int) return LL_VSC;
   function vsplth (A : LL_VSS; B : c_int) return LL_VSS;
   function vspltw (A : LL_VSI; B : c_int) return LL_VSI;

   function vspltisb (A : c_int) return LL_VSC;
   function vspltish (A : c_int) return LL_VSS;
   function vspltisw (A : c_int) return LL_VSI;

   function vsrb  (A : LL_VSC; B : LL_VSC) return LL_VSC;
   function vsrh  (A : LL_VSS; B : LL_VSS) return LL_VSS;
   function vsrw  (A : LL_VSI; B : LL_VSI) return LL_VSI;

   function vsrab (A : LL_VSC; B : LL_VSC) return LL_VSC;
   function vsrah (A : LL_VSS; B : LL_VSS) return LL_VSS;
   function vsraw (A : LL_VSI; B : LL_VSI) return LL_VSI;

   function vsr   (A : LL_VSI; B : LL_VSI) return LL_VSI;
   function vsro  (A : LL_VSI; B : LL_VSI) return LL_VSI;

   procedure stvx   (A : LL_VSI; B : c_int; C : c_ptr);
   procedure stvebx (A : LL_VSC; B : c_int; C : c_ptr);
   procedure stvehx (A : LL_VSS; B : c_int; C : c_ptr);
   procedure stvewx (A : LL_VSI; B : c_int; C : c_ptr);
   procedure stvxl  (A : LL_VSI; B : c_int; C : c_ptr);

   function vsububm (A : LL_VSC; B : LL_VSC) return LL_VSC;
   function vsubuhm (A : LL_VSS; B : LL_VSS) return LL_VSS;
   function vsubuwm (A : LL_VSI; B : LL_VSI) return LL_VSI;
   function vsubfp  (A : LL_VF;  B : LL_VF)  return LL_VF;

   function vsubcuw (A : LL_VSI; B : LL_VSI) return LL_VSI;

   function vsububs (A : LL_VSC; B : LL_VSC) return LL_VSC;
   function vsubsbs (A : LL_VSC; B : LL_VSC) return LL_VSC;
   function vsubuhs (A : LL_VSS; B : LL_VSS) return LL_VSS;
   function vsubshs (A : LL_VSS; B : LL_VSS) return LL_VSS;
   function vsubuws (A : LL_VSI; B : LL_VSI) return LL_VSI;
   function vsubsws (A : LL_VSI; B : LL_VSI) return LL_VSI;

   function vsum4ubs (A : LL_VSC; B : LL_VSI) return LL_VSI;
   function vsum4sbs (A : LL_VSC; B : LL_VSI) return LL_VSI;
   function vsum4shs (A : LL_VSS; B : LL_VSI) return LL_VSI;

   function vsum2sws (A : LL_VSI; B : LL_VSI) return LL_VSI;
   function vsumsws  (A : LL_VSI; B : LL_VSI) return LL_VSI;

   function vupkhsb (A : LL_VSC) return LL_VSS;
   function vupkhsh (A : LL_VSS) return LL_VSI;
   function vupkhpx (A : LL_VSS) return LL_VSI;

   function vupklsb (A : LL_VSC) return LL_VSS;
   function vupklsh (A : LL_VSS) return LL_VSI;
   function vupklpx (A : LL_VSS) return LL_VSI;

   function vcmpequb_p (A : c_int; B : LL_VSC; C : LL_VSC) return c_int;
   function vcmpequh_p (A : c_int; B : LL_VSS; C : LL_VSS) return c_int;
   function vcmpequw_p (A : c_int; B : LL_VSI; C : LL_VSI) return c_int;
   function vcmpeqfp_p (A : c_int; B : LL_VF; C : LL_VF) return c_int;

   function vcmpgtub_p (A : c_int; B : LL_VSC; C : LL_VSC) return c_int;
   function vcmpgtuh_p (A : c_int; B : LL_VSS; C : LL_VSS) return c_int;
   function vcmpgtuw_p (A : c_int; B : LL_VSI; C : LL_VSI) return c_int;
   function vcmpgtsb_p (A : c_int; B : LL_VSC; C : LL_VSC) return c_int;
   function vcmpgtsh_p (A : c_int; B : LL_VSS; C : LL_VSS) return c_int;
   function vcmpgtsw_p (A : c_int; B : LL_VSI; C : LL_VSI) return c_int;
   function vcmpgtfp_p (A : c_int; B : LL_VF; C : LL_VF) return c_int;

   function vcmpgefp_p (A : c_int; B : LL_VF; C : LL_VF) return c_int;
   function vcmpbfp_p  (A : c_int; B : LL_VF; C : LL_VF) return c_int;

private

   ---------------------------------------
   -- Low level vector type definitions --
   ---------------------------------------

   --  We simply use the natural array definitions corresponding to each
   --  user-level vector type. We need to put pragma Universal_Aliasing
   --  on these types because the common operations are implemented by
   --  means of Unchecked_Conversion betwwen different representations.

   --------------------------
   -- char Core Components --
   --------------------------

   type LL_VUC is new VUC_View;
   pragma Universal_Aliasing (LL_VUC);

   type LL_VSC is new VSC_View;
   pragma Universal_Aliasing (LL_VSC);

   type LL_VBC is new VBC_View;
   pragma Universal_Aliasing (LL_VBC);

   ---------------------------
   -- short Core Components --
   ---------------------------

   type LL_VUS is new VUS_View;
   pragma Universal_Aliasing (LL_VUS);

   type LL_VSS is new VSS_View;
   pragma Universal_Aliasing (LL_VSS);

   type LL_VBS is new VBS_View;
   pragma Universal_Aliasing (LL_VBS);

   -------------------------
   -- int Core Components --
   -------------------------

   type LL_VUI is new VUI_View;
   pragma Universal_Aliasing (LL_VUI);

   type LL_VSI is new VSI_View;
   pragma Universal_Aliasing (LL_VSI);

   type LL_VBI is new VBI_View;
   pragma Universal_Aliasing (LL_VBI);

   ---------------------------
   -- Float Core Components --
   ---------------------------

   type LL_VF is new VF_View;
   pragma Universal_Aliasing (LL_VF);

   ---------------------------
   -- pixel Core Components --
   ---------------------------

   type LL_VP is new VP_View;
   pragma Universal_Aliasing (LL_VP);

   ------------------------------------
   -- Low level functional interface --
   ------------------------------------

   pragma Convention_Identifier (LL_Altivec, C);

   pragma Export (LL_Altivec, dss,         "__builtin_altivec_dss");
   pragma Export (LL_Altivec, dssall,      "__builtin_altivec_dssall");
   pragma Export (LL_Altivec, dst,         "__builtin_altivec_dst");
   pragma Export (LL_Altivec, dstst,       "__builtin_altivec_dstst");
   pragma Export (LL_Altivec, dststt,      "__builtin_altivec_dststt");
   pragma Export (LL_Altivec, dstt,        "__builtin_altivec_dstt");
   pragma Export (LL_Altivec, mtvscr,      "__builtin_altivec_mtvscr");
   pragma Export (LL_Altivec, mfvscr,      "__builtin_altivec_mfvscr");
   pragma Export (LL_Altivec, stvebx,      "__builtin_altivec_stvebx");
   pragma Export (LL_Altivec, stvehx,      "__builtin_altivec_stvehx");
   pragma Export (LL_Altivec, stvewx,      "__builtin_altivec_stvewx");
   pragma Export (LL_Altivec, stvx,        "__builtin_altivec_stvx");
   pragma Export (LL_Altivec, stvxl,       "__builtin_altivec_stvxl");
   pragma Export (LL_Altivec, lvebx,       "__builtin_altivec_lvebx");
   pragma Export (LL_Altivec, lvehx,       "__builtin_altivec_lvehx");
   pragma Export (LL_Altivec, lvewx,       "__builtin_altivec_lvewx");
   pragma Export (LL_Altivec, lvx,         "__builtin_altivec_lvx");
   pragma Export (LL_Altivec, lvxl,        "__builtin_altivec_lvxl");
   pragma Export (LL_Altivec, lvsl,        "__builtin_altivec_lvsl");
   pragma Export (LL_Altivec, lvsr,        "__builtin_altivec_lvsr");
   pragma Export (LL_Altivec, abs_v16qi,   "__builtin_altivec_abs_v16qi");
   pragma Export (LL_Altivec, abs_v8hi,    "__builtin_altivec_abs_v8hi");
   pragma Export (LL_Altivec, abs_v4si,    "__builtin_altivec_abs_v4si");
   pragma Export (LL_Altivec, abs_v4sf,    "__builtin_altivec_abs_v4sf");
   pragma Export (LL_Altivec, abss_v16qi,  "__builtin_altivec_abss_v16qi");
   pragma Export (LL_Altivec, abss_v8hi,   "__builtin_altivec_abss_v8hi");
   pragma Export (LL_Altivec, abss_v4si,   "__builtin_altivec_abss_v4si");
   pragma Export (LL_Altivec, vaddcuw,     "__builtin_altivec_vaddcuw");
   pragma Export (LL_Altivec, vaddfp,      "__builtin_altivec_vaddfp");
   pragma Export (LL_Altivec, vaddsbs,     "__builtin_altivec_vaddsbs");
   pragma Export (LL_Altivec, vaddshs,     "__builtin_altivec_vaddshs");
   pragma Export (LL_Altivec, vaddsws,     "__builtin_altivec_vaddsws");
   pragma Export (LL_Altivec, vaddubm,     "__builtin_altivec_vaddubm");
   pragma Export (LL_Altivec, vaddubs,     "__builtin_altivec_vaddubs");
   pragma Export (LL_Altivec, vadduhm,     "__builtin_altivec_vadduhm");
   pragma Export (LL_Altivec, vadduhs,     "__builtin_altivec_vadduhs");
   pragma Export (LL_Altivec, vadduwm,     "__builtin_altivec_vadduwm");
   pragma Export (LL_Altivec, vadduws,     "__builtin_altivec_vadduws");
   pragma Export (LL_Altivec, vand,        "__builtin_altivec_vand");
   pragma Export (LL_Altivec, vandc,       "__builtin_altivec_vandc");
   pragma Export (LL_Altivec, vavgsb,      "__builtin_altivec_vavgsb");
   pragma Export (LL_Altivec, vavgsh,      "__builtin_altivec_vavgsh");
   pragma Export (LL_Altivec, vavgsw,      "__builtin_altivec_vavgsw");
   pragma Export (LL_Altivec, vavgub,      "__builtin_altivec_vavgub");
   pragma Export (LL_Altivec, vavguh,      "__builtin_altivec_vavguh");
   pragma Export (LL_Altivec, vavguw,      "__builtin_altivec_vavguw");
   pragma Export (LL_Altivec, vcfsx,       "__builtin_altivec_vcfsx");
   pragma Export (LL_Altivec, vcfux,       "__builtin_altivec_vcfux");
   pragma Export (LL_Altivec, vcmpbfp,     "__builtin_altivec_vcmpbfp");
   pragma Export (LL_Altivec, vcmpeqfp,    "__builtin_altivec_vcmpeqfp");
   pragma Export (LL_Altivec, vcmpequb,    "__builtin_altivec_vcmpequb");
   pragma Export (LL_Altivec, vcmpequh,    "__builtin_altivec_vcmpequh");
   pragma Export (LL_Altivec, vcmpequw,    "__builtin_altivec_vcmpequw");
   pragma Export (LL_Altivec, vcmpgefp,    "__builtin_altivec_vcmpgefp");
   pragma Export (LL_Altivec, vcmpgtfp,    "__builtin_altivec_vcmpgtfp");
   pragma Export (LL_Altivec, vcmpgtsb,    "__builtin_altivec_vcmpgtsb");
   pragma Export (LL_Altivec, vcmpgtsh,    "__builtin_altivec_vcmpgtsh");
   pragma Export (LL_Altivec, vcmpgtsw,    "__builtin_altivec_vcmpgtsw");
   pragma Export (LL_Altivec, vcmpgtub,    "__builtin_altivec_vcmpgtub");
   pragma Export (LL_Altivec, vcmpgtuh,    "__builtin_altivec_vcmpgtuh");
   pragma Export (LL_Altivec, vcmpgtuw,    "__builtin_altivec_vcmpgtuw");
   pragma Export (LL_Altivec, vctsxs,      "__builtin_altivec_vctsxs");
   pragma Export (LL_Altivec, vctuxs,      "__builtin_altivec_vctuxs");
   pragma Export (LL_Altivec, vexptefp,    "__builtin_altivec_vexptefp");
   pragma Export (LL_Altivec, vlogefp,     "__builtin_altivec_vlogefp");
   pragma Export (LL_Altivec, vmaddfp,     "__builtin_altivec_vmaddfp");
   pragma Export (LL_Altivec, vmaxfp,      "__builtin_altivec_vmaxfp");
   pragma Export (LL_Altivec, vmaxsb,      "__builtin_altivec_vmaxsb");
   pragma Export (LL_Altivec, vmaxsh,      "__builtin_altivec_vmaxsh");
   pragma Export (LL_Altivec, vmaxsw,      "__builtin_altivec_vmaxsw");
   pragma Export (LL_Altivec, vmaxub,      "__builtin_altivec_vmaxub");
   pragma Export (LL_Altivec, vmaxuh,      "__builtin_altivec_vmaxuh");
   pragma Export (LL_Altivec, vmaxuw,      "__builtin_altivec_vmaxuw");
   pragma Export (LL_Altivec, vmhaddshs,   "__builtin_altivec_vmhaddshs");
   pragma Export (LL_Altivec, vmhraddshs,  "__builtin_altivec_vmhraddshs");
   pragma Export (LL_Altivec, vminfp,      "__builtin_altivec_vminfp");
   pragma Export (LL_Altivec, vminsb,      "__builtin_altivec_vminsb");
   pragma Export (LL_Altivec, vminsh,      "__builtin_altivec_vminsh");
   pragma Export (LL_Altivec, vminsw,      "__builtin_altivec_vminsw");
   pragma Export (LL_Altivec, vminub,      "__builtin_altivec_vminub");
   pragma Export (LL_Altivec, vminuh,      "__builtin_altivec_vminuh");
   pragma Export (LL_Altivec, vminuw,      "__builtin_altivec_vminuw");
   pragma Export (LL_Altivec, vmladduhm,   "__builtin_altivec_vmladduhm");
   pragma Export (LL_Altivec, vmrghb,      "__builtin_altivec_vmrghb");
   pragma Export (LL_Altivec, vmrghh,      "__builtin_altivec_vmrghh");
   pragma Export (LL_Altivec, vmrghw,      "__builtin_altivec_vmrghw");
   pragma Export (LL_Altivec, vmrglb,      "__builtin_altivec_vmrglb");
   pragma Export (LL_Altivec, vmrglh,      "__builtin_altivec_vmrglh");
   pragma Export (LL_Altivec, vmrglw,      "__builtin_altivec_vmrglw");
   pragma Export (LL_Altivec, vmsummbm,    "__builtin_altivec_vmsummbm");
   pragma Export (LL_Altivec, vmsumshm,    "__builtin_altivec_vmsumshm");
   pragma Export (LL_Altivec, vmsumshs,    "__builtin_altivec_vmsumshs");
   pragma Export (LL_Altivec, vmsumubm,    "__builtin_altivec_vmsumubm");
   pragma Export (LL_Altivec, vmsumuhm,    "__builtin_altivec_vmsumuhm");
   pragma Export (LL_Altivec, vmsumuhs,    "__builtin_altivec_vmsumuhs");
   pragma Export (LL_Altivec, vmulesb,     "__builtin_altivec_vmulesb");
   pragma Export (LL_Altivec, vmulesh,     "__builtin_altivec_vmulesh");
   pragma Export (LL_Altivec, vmuleub,     "__builtin_altivec_vmuleub");
   pragma Export (LL_Altivec, vmuleuh,     "__builtin_altivec_vmuleuh");
   pragma Export (LL_Altivec, vmulosb,     "__builtin_altivec_vmulosb");
   pragma Export (LL_Altivec, vmulosh,     "__builtin_altivec_vmulosh");
   pragma Export (LL_Altivec, vmuloub,     "__builtin_altivec_vmuloub");
   pragma Export (LL_Altivec, vmulouh,     "__builtin_altivec_vmulouh");
   pragma Export (LL_Altivec, vnmsubfp,    "__builtin_altivec_vnmsubfp");
   pragma Export (LL_Altivec, vnor,        "__builtin_altivec_vnor");
   pragma Export (LL_Altivec, vxor,        "__builtin_altivec_vxor");
   pragma Export (LL_Altivec, vor,         "__builtin_altivec_vor");
   pragma Export (LL_Altivec, vperm_4si,   "__builtin_altivec_vperm_4si");
   pragma Export (LL_Altivec, vpkpx,       "__builtin_altivec_vpkpx");
   pragma Export (LL_Altivec, vpkshss,     "__builtin_altivec_vpkshss");
   pragma Export (LL_Altivec, vpkshus,     "__builtin_altivec_vpkshus");
   pragma Export (LL_Altivec, vpkswss,     "__builtin_altivec_vpkswss");
   pragma Export (LL_Altivec, vpkswus,     "__builtin_altivec_vpkswus");
   pragma Export (LL_Altivec, vpkuhum,     "__builtin_altivec_vpkuhum");
   pragma Export (LL_Altivec, vpkuhus,     "__builtin_altivec_vpkuhus");
   pragma Export (LL_Altivec, vpkuwum,     "__builtin_altivec_vpkuwum");
   pragma Export (LL_Altivec, vpkuwus,     "__builtin_altivec_vpkuwus");
   pragma Export (LL_Altivec, vrefp,       "__builtin_altivec_vrefp");
   pragma Export (LL_Altivec, vrfim,       "__builtin_altivec_vrfim");
   pragma Export (LL_Altivec, vrfin,       "__builtin_altivec_vrfin");
   pragma Export (LL_Altivec, vrfip,       "__builtin_altivec_vrfip");
   pragma Export (LL_Altivec, vrfiz,       "__builtin_altivec_vrfiz");
   pragma Export (LL_Altivec, vrlb,        "__builtin_altivec_vrlb");
   pragma Export (LL_Altivec, vrlh,        "__builtin_altivec_vrlh");
   pragma Export (LL_Altivec, vrlw,        "__builtin_altivec_vrlw");
   pragma Export (LL_Altivec, vrsqrtefp,   "__builtin_altivec_vrsqrtefp");
   pragma Export (LL_Altivec, vsel_4si,    "__builtin_altivec_vsel_4si");
   pragma Export (LL_Altivec, vsldoi_4si,  "__builtin_altivec_vsldoi_4si");
   pragma Export (LL_Altivec, vsldoi_8hi,  "__builtin_altivec_vsldoi_8hi");
   pragma Export (LL_Altivec, vsldoi_16qi, "__builtin_altivec_vsldoi_16qi");
   pragma Export (LL_Altivec, vsldoi_4sf,  "__builtin_altivec_vsldoi_4sf");
   pragma Export (LL_Altivec, vsl,         "__builtin_altivec_vsl");
   pragma Export (LL_Altivec, vslb,        "__builtin_altivec_vslb");
   pragma Export (LL_Altivec, vslh,        "__builtin_altivec_vslh");
   pragma Export (LL_Altivec, vslo,        "__builtin_altivec_vslo");
   pragma Export (LL_Altivec, vslw,        "__builtin_altivec_vslw");
   pragma Export (LL_Altivec, vspltb,      "__builtin_altivec_vspltb");
   pragma Export (LL_Altivec, vsplth,      "__builtin_altivec_vsplth");
   pragma Export (LL_Altivec, vspltisb,    "__builtin_altivec_vspltisb");
   pragma Export (LL_Altivec, vspltish,    "__builtin_altivec_vspltish");
   pragma Export (LL_Altivec, vspltisw,    "__builtin_altivec_vspltisw");
   pragma Export (LL_Altivec, vspltw,      "__builtin_altivec_vspltw");
   pragma Export (LL_Altivec, vsr,         "__builtin_altivec_vsr");
   pragma Export (LL_Altivec, vsrab,       "__builtin_altivec_vsrab");
   pragma Export (LL_Altivec, vsrah,       "__builtin_altivec_vsrah");
   pragma Export (LL_Altivec, vsraw,       "__builtin_altivec_vsraw");
   pragma Export (LL_Altivec, vsrb,        "__builtin_altivec_vsrb");
   pragma Export (LL_Altivec, vsrh,        "__builtin_altivec_vsrh");
   pragma Export (LL_Altivec, vsro,        "__builtin_altivec_vsro");
   pragma Export (LL_Altivec, vsrw,        "__builtin_altivec_vsrw");
   pragma Export (LL_Altivec, vsubcuw,     "__builtin_altivec_vsubcuw");
   pragma Export (LL_Altivec, vsubfp,      "__builtin_altivec_vsubfp");
   pragma Export (LL_Altivec, vsubsbs,     "__builtin_altivec_vsubsbs");
   pragma Export (LL_Altivec, vsubshs,     "__builtin_altivec_vsubshs");
   pragma Export (LL_Altivec, vsubsws,     "__builtin_altivec_vsubsws");
   pragma Export (LL_Altivec, vsububm,     "__builtin_altivec_vsububm");
   pragma Export (LL_Altivec, vsububs,     "__builtin_altivec_vsububs");
   pragma Export (LL_Altivec, vsubuhm,     "__builtin_altivec_vsubuhm");
   pragma Export (LL_Altivec, vsubuhs,     "__builtin_altivec_vsubuhs");
   pragma Export (LL_Altivec, vsubuwm,     "__builtin_altivec_vsubuwm");
   pragma Export (LL_Altivec, vsubuws,     "__builtin_altivec_vsubuws");
   pragma Export (LL_Altivec, vsum2sws,    "__builtin_altivec_vsum2sws");
   pragma Export (LL_Altivec, vsum4sbs,    "__builtin_altivec_vsum4sbs");
   pragma Export (LL_Altivec, vsum4shs,    "__builtin_altivec_vsum4shs");
   pragma Export (LL_Altivec, vsum4ubs,    "__builtin_altivec_vsum4ubs");
   pragma Export (LL_Altivec, vsumsws,     "__builtin_altivec_vsumsws");
   pragma Export (LL_Altivec, vupkhpx,     "__builtin_altivec_vupkhpx");
   pragma Export (LL_Altivec, vupkhsb,     "__builtin_altivec_vupkhsb");
   pragma Export (LL_Altivec, vupkhsh,     "__builtin_altivec_vupkhsh");
   pragma Export (LL_Altivec, vupklpx,     "__builtin_altivec_vupklpx");
   pragma Export (LL_Altivec, vupklsb,     "__builtin_altivec_vupklsb");
   pragma Export (LL_Altivec, vupklsh,     "__builtin_altivec_vupklsh");
   pragma Export (LL_Altivec, vcmpbfp_p,   "__builtin_altivec_vcmpbfp_p");
   pragma Export (LL_Altivec, vcmpeqfp_p,  "__builtin_altivec_vcmpeqfp_p");
   pragma Export (LL_Altivec, vcmpgefp_p,  "__builtin_altivec_vcmpgefp_p");
   pragma Export (LL_Altivec, vcmpgtfp_p,  "__builtin_altivec_vcmpgtfp_p");
   pragma Export (LL_Altivec, vcmpequw_p,  "__builtin_altivec_vcmpequw_p");
   pragma Export (LL_Altivec, vcmpgtsw_p,  "__builtin_altivec_vcmpgtsw_p");
   pragma Export (LL_Altivec, vcmpgtuw_p,  "__builtin_altivec_vcmpgtuw_p");
   pragma Export (LL_Altivec, vcmpgtuh_p,  "__builtin_altivec_vcmpgtuh_p");
   pragma Export (LL_Altivec, vcmpgtsh_p,  "__builtin_altivec_vcmpgtsh_p");
   pragma Export (LL_Altivec, vcmpequh_p,  "__builtin_altivec_vcmpequh_p");
   pragma Export (LL_Altivec, vcmpequb_p,  "__builtin_altivec_vcmpequb_p");
   pragma Export (LL_Altivec, vcmpgtsb_p,  "__builtin_altivec_vcmpgtsb_p");
   pragma Export (LL_Altivec, vcmpgtub_p,  "__builtin_altivec_vcmpgtub_p");

end GNAT.Altivec.Low_Level_Vectors;
