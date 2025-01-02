------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--       G N A T . A L T I V E C . L O W _ L E V E L _ V E C T O R S        --
--                                                                          --
--                                 S p e c                                  --
--                          (Hard Binding Version)                          --
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

--  This unit exposes the low level vector support for the Hard binding,
--  intended for AltiVec capable targets. See Altivec.Design for a description
--  of what is expected to be exposed.

package GNAT.Altivec.Low_Level_Vectors is
   pragma Elaborate_Body;

   ----------------------------------------
   -- Low-level Vector Type Declarations --
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
   -- Low-level Functional Interface --
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
   function vcmpeqfp (A : LL_VF;  B : LL_VF)  return LL_VF;

   function vcmpgefp (A : LL_VF; B : LL_VF) return LL_VF;

   function vcmpgtub (A : LL_VSC; B : LL_VSC) return LL_VSC;
   function vcmpgtsb (A : LL_VSC; B : LL_VSC) return LL_VSC;
   function vcmpgtuh (A : LL_VSS; B : LL_VSS) return LL_VSS;
   function vcmpgtsh (A : LL_VSS; B : LL_VSS) return LL_VSS;
   function vcmpgtuw (A : LL_VSI; B : LL_VSI) return LL_VSI;
   function vcmpgtsw (A : LL_VSI; B : LL_VSI) return LL_VSI;
   function vcmpgtfp (A : LL_VF;  B : LL_VF)  return LL_VF;

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
   function vcmpgtfp_p (A : c_int; B : LL_VF;  C : LL_VF)  return c_int;

   function vcmpgefp_p (A : c_int; B : LL_VF; C : LL_VF) return c_int;
   function vcmpbfp_p  (A : c_int; B : LL_VF; C : LL_VF) return c_int;

private

   ---------------------------------------
   -- Low-level Vector Type Definitions --
   ---------------------------------------

   --  [PIM-2.3.3 Alignment of aggregate and unions containing vector types]:

   --     "Aggregates (structures and arrays) and unions containing vector
   --      types must be aligned on 16-byte boundaries and their internal
   --      organization padded, if necessary, so that each internal vector
   --      type is aligned on a 16-byte boundary. This is an extension to
   --      all ABIs (AIX, Apple, SVR4, and EABI).

   --------------------------
   -- char Core Components --
   --------------------------

   type LL_VUC is array (1 .. 16) of unsigned_char;
   for LL_VUC'Alignment use VECTOR_ALIGNMENT;
   pragma Machine_Attribute (LL_VUC, "vector_type");
   pragma Universal_Aliasing (LL_VUC);
   pragma Suppress (All_Checks, LL_VUC);

   type LL_VSC is array (1 .. 16) of signed_char;
   for LL_VSC'Alignment use VECTOR_ALIGNMENT;
   pragma Machine_Attribute (LL_VSC, "vector_type");
   pragma Universal_Aliasing (LL_VSC);
   pragma Suppress (All_Checks, LL_VSC);

   type LL_VBC is array (1 .. 16) of unsigned_char;
   for LL_VBC'Alignment use VECTOR_ALIGNMENT;
   pragma Machine_Attribute (LL_VBC, "vector_type");
   pragma Universal_Aliasing (LL_VBC);
   pragma Suppress (All_Checks, LL_VBC);

   ---------------------------
   -- short Core Components --
   ---------------------------

   type LL_VUS is array (1 .. 8) of unsigned_short;
   for LL_VUS'Alignment use VECTOR_ALIGNMENT;
   pragma Machine_Attribute (LL_VUS, "vector_type");
   pragma Universal_Aliasing (LL_VUS);
   pragma Suppress (All_Checks, LL_VUS);

   type LL_VSS is array (1 .. 8) of signed_short;
   for LL_VSS'Alignment use VECTOR_ALIGNMENT;
   pragma Machine_Attribute (LL_VSS, "vector_type");
   pragma Universal_Aliasing (LL_VSS);
   pragma Suppress (All_Checks, LL_VSS);

   type LL_VBS is array (1 .. 8) of unsigned_short;
   for LL_VBS'Alignment use VECTOR_ALIGNMENT;
   pragma Machine_Attribute (LL_VBS, "vector_type");
   pragma Universal_Aliasing (LL_VBS);
   pragma Suppress (All_Checks, LL_VBS);

   -------------------------
   -- int Core Components --
   -------------------------

   type LL_VUI is array (1 .. 4) of unsigned_int;
   for LL_VUI'Alignment use VECTOR_ALIGNMENT;
   pragma Machine_Attribute (LL_VUI, "vector_type");
   pragma Universal_Aliasing (LL_VUI);
   pragma Suppress (All_Checks, LL_VUI);

   type LL_VSI is array (1 .. 4) of signed_int;
   for LL_VSI'Alignment use VECTOR_ALIGNMENT;
   pragma Machine_Attribute (LL_VSI, "vector_type");
   pragma Universal_Aliasing (LL_VSI);
   pragma Suppress (All_Checks, LL_VSI);

   type LL_VBI is array (1 .. 4) of unsigned_int;
   for LL_VBI'Alignment use VECTOR_ALIGNMENT;
   pragma Machine_Attribute (LL_VBI, "vector_type");
   pragma Universal_Aliasing (LL_VBI);
   pragma Suppress (All_Checks, LL_VBI);

   ---------------------------
   -- Float Core Components --
   ---------------------------

   type LL_VF is array (1 .. 4) of Float;
   for LL_VF'Alignment use VECTOR_ALIGNMENT;
   pragma Machine_Attribute (LL_VF, "vector_type");
   pragma Universal_Aliasing (LL_VF);
   pragma Suppress (All_Checks, LL_VF);

   ---------------------------
   -- pixel Core Components --
   ---------------------------

   type LL_VP is array (1 .. 8) of pixel;
   for LL_VP'Alignment use VECTOR_ALIGNMENT;
   pragma Machine_Attribute (LL_VP, "vector_type");
   pragma Universal_Aliasing (LL_VP);
   pragma Suppress (All_Checks, LL_VP);

   ------------------------------------
   -- Low-level Functional Interface --
   ------------------------------------

   --  The functions we have to expose here are exactly those for which
   --  GCC builtins are available. Calls to these functions will be turned
   --  into real AltiVec instructions by the GCC back-end.

   pragma Convention_Identifier (LL_Altivec, Intrinsic);

   pragma Import (LL_Altivec, dss,         "__builtin_altivec_dss");
   pragma Import (LL_Altivec, dssall,      "__builtin_altivec_dssall");
   pragma Import (LL_Altivec, dst,         "__builtin_altivec_dst");
   pragma Import (LL_Altivec, dstst,       "__builtin_altivec_dstst");
   pragma Import (LL_Altivec, dststt,      "__builtin_altivec_dststt");
   pragma Import (LL_Altivec, dstt,        "__builtin_altivec_dstt");
   pragma Import (LL_Altivec, mtvscr,      "__builtin_altivec_mtvscr");
   pragma Import (LL_Altivec, mfvscr,      "__builtin_altivec_mfvscr");
   pragma Import (LL_Altivec, stvebx,      "__builtin_altivec_stvebx");
   pragma Import (LL_Altivec, stvehx,      "__builtin_altivec_stvehx");
   pragma Import (LL_Altivec, stvewx,      "__builtin_altivec_stvewx");
   pragma Import (LL_Altivec, stvx,        "__builtin_altivec_stvx");
   pragma Import (LL_Altivec, stvxl,       "__builtin_altivec_stvxl");
   pragma Import (LL_Altivec, lvebx,       "__builtin_altivec_lvebx");
   pragma Import (LL_Altivec, lvehx,       "__builtin_altivec_lvehx");
   pragma Import (LL_Altivec, lvewx,       "__builtin_altivec_lvewx");
   pragma Import (LL_Altivec, lvx,         "__builtin_altivec_lvx");
   pragma Import (LL_Altivec, lvxl,        "__builtin_altivec_lvxl");
   pragma Import (LL_Altivec, lvsl,        "__builtin_altivec_lvsl");
   pragma Import (LL_Altivec, lvsr,        "__builtin_altivec_lvsr");
   pragma Import (LL_Altivec, abs_v16qi,   "__builtin_altivec_abs_v16qi");
   pragma Import (LL_Altivec, abs_v8hi,    "__builtin_altivec_abs_v8hi");
   pragma Import (LL_Altivec, abs_v4si,    "__builtin_altivec_abs_v4si");
   pragma Import (LL_Altivec, abs_v4sf,    "__builtin_altivec_abs_v4sf");
   pragma Import (LL_Altivec, abss_v16qi,  "__builtin_altivec_abss_v16qi");
   pragma Import (LL_Altivec, abss_v8hi,   "__builtin_altivec_abss_v8hi");
   pragma Import (LL_Altivec, abss_v4si,   "__builtin_altivec_abss_v4si");
   pragma Import (LL_Altivec, vaddcuw,     "__builtin_altivec_vaddcuw");
   pragma Import (LL_Altivec, vaddfp,      "__builtin_altivec_vaddfp");
   pragma Import (LL_Altivec, vaddsbs,     "__builtin_altivec_vaddsbs");
   pragma Import (LL_Altivec, vaddshs,     "__builtin_altivec_vaddshs");
   pragma Import (LL_Altivec, vaddsws,     "__builtin_altivec_vaddsws");
   pragma Import (LL_Altivec, vaddubm,     "__builtin_altivec_vaddubm");
   pragma Import (LL_Altivec, vaddubs,     "__builtin_altivec_vaddubs");
   pragma Import (LL_Altivec, vadduhm,     "__builtin_altivec_vadduhm");
   pragma Import (LL_Altivec, vadduhs,     "__builtin_altivec_vadduhs");
   pragma Import (LL_Altivec, vadduwm,     "__builtin_altivec_vadduwm");
   pragma Import (LL_Altivec, vadduws,     "__builtin_altivec_vadduws");
   pragma Import (LL_Altivec, vand,        "__builtin_altivec_vand");
   pragma Import (LL_Altivec, vandc,       "__builtin_altivec_vandc");
   pragma Import (LL_Altivec, vavgsb,      "__builtin_altivec_vavgsb");
   pragma Import (LL_Altivec, vavgsh,      "__builtin_altivec_vavgsh");
   pragma Import (LL_Altivec, vavgsw,      "__builtin_altivec_vavgsw");
   pragma Import (LL_Altivec, vavgub,      "__builtin_altivec_vavgub");
   pragma Import (LL_Altivec, vavguh,      "__builtin_altivec_vavguh");
   pragma Import (LL_Altivec, vavguw,      "__builtin_altivec_vavguw");
   pragma Import (LL_Altivec, vcfsx,       "__builtin_altivec_vcfsx");
   pragma Import (LL_Altivec, vcfux,       "__builtin_altivec_vcfux");
   pragma Import (LL_Altivec, vcmpbfp,     "__builtin_altivec_vcmpbfp");
   pragma Import (LL_Altivec, vcmpeqfp,    "__builtin_altivec_vcmpeqfp");
   pragma Import (LL_Altivec, vcmpequb,    "__builtin_altivec_vcmpequb");
   pragma Import (LL_Altivec, vcmpequh,    "__builtin_altivec_vcmpequh");
   pragma Import (LL_Altivec, vcmpequw,    "__builtin_altivec_vcmpequw");
   pragma Import (LL_Altivec, vcmpgefp,    "__builtin_altivec_vcmpgefp");
   pragma Import (LL_Altivec, vcmpgtfp,    "__builtin_altivec_vcmpgtfp");
   pragma Import (LL_Altivec, vcmpgtsb,    "__builtin_altivec_vcmpgtsb");
   pragma Import (LL_Altivec, vcmpgtsh,    "__builtin_altivec_vcmpgtsh");
   pragma Import (LL_Altivec, vcmpgtsw,    "__builtin_altivec_vcmpgtsw");
   pragma Import (LL_Altivec, vcmpgtub,    "__builtin_altivec_vcmpgtub");
   pragma Import (LL_Altivec, vcmpgtuh,    "__builtin_altivec_vcmpgtuh");
   pragma Import (LL_Altivec, vcmpgtuw,    "__builtin_altivec_vcmpgtuw");
   pragma Import (LL_Altivec, vctsxs,      "__builtin_altivec_vctsxs");
   pragma Import (LL_Altivec, vctuxs,      "__builtin_altivec_vctuxs");
   pragma Import (LL_Altivec, vexptefp,    "__builtin_altivec_vexptefp");
   pragma Import (LL_Altivec, vlogefp,     "__builtin_altivec_vlogefp");
   pragma Import (LL_Altivec, vmaddfp,     "__builtin_altivec_vmaddfp");
   pragma Import (LL_Altivec, vmaxfp,      "__builtin_altivec_vmaxfp");
   pragma Import (LL_Altivec, vmaxsb,      "__builtin_altivec_vmaxsb");
   pragma Import (LL_Altivec, vmaxsh,      "__builtin_altivec_vmaxsh");
   pragma Import (LL_Altivec, vmaxsw,      "__builtin_altivec_vmaxsw");
   pragma Import (LL_Altivec, vmaxub,      "__builtin_altivec_vmaxub");
   pragma Import (LL_Altivec, vmaxuh,      "__builtin_altivec_vmaxuh");
   pragma Import (LL_Altivec, vmaxuw,      "__builtin_altivec_vmaxuw");
   pragma Import (LL_Altivec, vmhaddshs,   "__builtin_altivec_vmhaddshs");
   pragma Import (LL_Altivec, vmhraddshs,  "__builtin_altivec_vmhraddshs");
   pragma Import (LL_Altivec, vminfp,      "__builtin_altivec_vminfp");
   pragma Import (LL_Altivec, vminsb,      "__builtin_altivec_vminsb");
   pragma Import (LL_Altivec, vminsh,      "__builtin_altivec_vminsh");
   pragma Import (LL_Altivec, vminsw,      "__builtin_altivec_vminsw");
   pragma Import (LL_Altivec, vminub,      "__builtin_altivec_vminub");
   pragma Import (LL_Altivec, vminuh,      "__builtin_altivec_vminuh");
   pragma Import (LL_Altivec, vminuw,      "__builtin_altivec_vminuw");
   pragma Import (LL_Altivec, vmladduhm,   "__builtin_altivec_vmladduhm");
   pragma Import (LL_Altivec, vmrghb,      "__builtin_altivec_vmrghb");
   pragma Import (LL_Altivec, vmrghh,      "__builtin_altivec_vmrghh");
   pragma Import (LL_Altivec, vmrghw,      "__builtin_altivec_vmrghw");
   pragma Import (LL_Altivec, vmrglb,      "__builtin_altivec_vmrglb");
   pragma Import (LL_Altivec, vmrglh,      "__builtin_altivec_vmrglh");
   pragma Import (LL_Altivec, vmrglw,      "__builtin_altivec_vmrglw");
   pragma Import (LL_Altivec, vmsummbm,    "__builtin_altivec_vmsummbm");
   pragma Import (LL_Altivec, vmsumshm,    "__builtin_altivec_vmsumshm");
   pragma Import (LL_Altivec, vmsumshs,    "__builtin_altivec_vmsumshs");
   pragma Import (LL_Altivec, vmsumubm,    "__builtin_altivec_vmsumubm");
   pragma Import (LL_Altivec, vmsumuhm,    "__builtin_altivec_vmsumuhm");
   pragma Import (LL_Altivec, vmsumuhs,    "__builtin_altivec_vmsumuhs");
   pragma Import (LL_Altivec, vmulesb,     "__builtin_altivec_vmulesb");
   pragma Import (LL_Altivec, vmulesh,     "__builtin_altivec_vmulesh");
   pragma Import (LL_Altivec, vmuleub,     "__builtin_altivec_vmuleub");
   pragma Import (LL_Altivec, vmuleuh,     "__builtin_altivec_vmuleuh");
   pragma Import (LL_Altivec, vmulosb,     "__builtin_altivec_vmulosb");
   pragma Import (LL_Altivec, vmulosh,     "__builtin_altivec_vmulosh");
   pragma Import (LL_Altivec, vmuloub,     "__builtin_altivec_vmuloub");
   pragma Import (LL_Altivec, vmulouh,     "__builtin_altivec_vmulouh");
   pragma Import (LL_Altivec, vnmsubfp,    "__builtin_altivec_vnmsubfp");
   pragma Import (LL_Altivec, vnor,        "__builtin_altivec_vnor");
   pragma Import (LL_Altivec, vxor,        "__builtin_altivec_vxor");
   pragma Import (LL_Altivec, vor,         "__builtin_altivec_vor");
   pragma Import (LL_Altivec, vperm_4si,   "__builtin_altivec_vperm_4si");
   pragma Import (LL_Altivec, vpkpx,       "__builtin_altivec_vpkpx");
   pragma Import (LL_Altivec, vpkshss,     "__builtin_altivec_vpkshss");
   pragma Import (LL_Altivec, vpkshus,     "__builtin_altivec_vpkshus");
   pragma Import (LL_Altivec, vpkswss,     "__builtin_altivec_vpkswss");
   pragma Import (LL_Altivec, vpkswus,     "__builtin_altivec_vpkswus");
   pragma Import (LL_Altivec, vpkuhum,     "__builtin_altivec_vpkuhum");
   pragma Import (LL_Altivec, vpkuhus,     "__builtin_altivec_vpkuhus");
   pragma Import (LL_Altivec, vpkuwum,     "__builtin_altivec_vpkuwum");
   pragma Import (LL_Altivec, vpkuwus,     "__builtin_altivec_vpkuwus");
   pragma Import (LL_Altivec, vrefp,       "__builtin_altivec_vrefp");
   pragma Import (LL_Altivec, vrfim,       "__builtin_altivec_vrfim");
   pragma Import (LL_Altivec, vrfin,       "__builtin_altivec_vrfin");
   pragma Import (LL_Altivec, vrfip,       "__builtin_altivec_vrfip");
   pragma Import (LL_Altivec, vrfiz,       "__builtin_altivec_vrfiz");
   pragma Import (LL_Altivec, vrlb,        "__builtin_altivec_vrlb");
   pragma Import (LL_Altivec, vrlh,        "__builtin_altivec_vrlh");
   pragma Import (LL_Altivec, vrlw,        "__builtin_altivec_vrlw");
   pragma Import (LL_Altivec, vrsqrtefp,   "__builtin_altivec_vrsqrtefp");
   pragma Import (LL_Altivec, vsel_4si,    "__builtin_altivec_vsel_4si");
   pragma Import (LL_Altivec, vsldoi_4si,  "__builtin_altivec_vsldoi_4si");
   pragma Import (LL_Altivec, vsldoi_8hi,  "__builtin_altivec_vsldoi_8hi");
   pragma Import (LL_Altivec, vsldoi_16qi, "__builtin_altivec_vsldoi_16qi");
   pragma Import (LL_Altivec, vsldoi_4sf,  "__builtin_altivec_vsldoi_4sf");
   pragma Import (LL_Altivec, vsl,         "__builtin_altivec_vsl");
   pragma Import (LL_Altivec, vslb,        "__builtin_altivec_vslb");
   pragma Import (LL_Altivec, vslh,        "__builtin_altivec_vslh");
   pragma Import (LL_Altivec, vslo,        "__builtin_altivec_vslo");
   pragma Import (LL_Altivec, vslw,        "__builtin_altivec_vslw");
   pragma Import (LL_Altivec, vspltb,      "__builtin_altivec_vspltb");
   pragma Import (LL_Altivec, vsplth,      "__builtin_altivec_vsplth");
   pragma Import (LL_Altivec, vspltisb,    "__builtin_altivec_vspltisb");
   pragma Import (LL_Altivec, vspltish,    "__builtin_altivec_vspltish");
   pragma Import (LL_Altivec, vspltisw,    "__builtin_altivec_vspltisw");
   pragma Import (LL_Altivec, vspltw,      "__builtin_altivec_vspltw");
   pragma Import (LL_Altivec, vsr,         "__builtin_altivec_vsr");
   pragma Import (LL_Altivec, vsrab,       "__builtin_altivec_vsrab");
   pragma Import (LL_Altivec, vsrah,       "__builtin_altivec_vsrah");
   pragma Import (LL_Altivec, vsraw,       "__builtin_altivec_vsraw");
   pragma Import (LL_Altivec, vsrb,        "__builtin_altivec_vsrb");
   pragma Import (LL_Altivec, vsrh,        "__builtin_altivec_vsrh");
   pragma Import (LL_Altivec, vsro,        "__builtin_altivec_vsro");
   pragma Import (LL_Altivec, vsrw,        "__builtin_altivec_vsrw");
   pragma Import (LL_Altivec, vsubcuw,     "__builtin_altivec_vsubcuw");
   pragma Import (LL_Altivec, vsubfp,      "__builtin_altivec_vsubfp");
   pragma Import (LL_Altivec, vsubsbs,     "__builtin_altivec_vsubsbs");
   pragma Import (LL_Altivec, vsubshs,     "__builtin_altivec_vsubshs");
   pragma Import (LL_Altivec, vsubsws,     "__builtin_altivec_vsubsws");
   pragma Import (LL_Altivec, vsububm,     "__builtin_altivec_vsububm");
   pragma Import (LL_Altivec, vsububs,     "__builtin_altivec_vsububs");
   pragma Import (LL_Altivec, vsubuhm,     "__builtin_altivec_vsubuhm");
   pragma Import (LL_Altivec, vsubuhs,     "__builtin_altivec_vsubuhs");
   pragma Import (LL_Altivec, vsubuwm,     "__builtin_altivec_vsubuwm");
   pragma Import (LL_Altivec, vsubuws,     "__builtin_altivec_vsubuws");
   pragma Import (LL_Altivec, vsum2sws,    "__builtin_altivec_vsum2sws");
   pragma Import (LL_Altivec, vsum4sbs,    "__builtin_altivec_vsum4sbs");
   pragma Import (LL_Altivec, vsum4shs,    "__builtin_altivec_vsum4shs");
   pragma Import (LL_Altivec, vsum4ubs,    "__builtin_altivec_vsum4ubs");
   pragma Import (LL_Altivec, vsumsws,     "__builtin_altivec_vsumsws");
   pragma Import (LL_Altivec, vupkhpx,     "__builtin_altivec_vupkhpx");
   pragma Import (LL_Altivec, vupkhsb,     "__builtin_altivec_vupkhsb");
   pragma Import (LL_Altivec, vupkhsh,     "__builtin_altivec_vupkhsh");
   pragma Import (LL_Altivec, vupklpx,     "__builtin_altivec_vupklpx");
   pragma Import (LL_Altivec, vupklsb,     "__builtin_altivec_vupklsb");
   pragma Import (LL_Altivec, vupklsh,     "__builtin_altivec_vupklsh");
   pragma Import (LL_Altivec, vcmpbfp_p,   "__builtin_altivec_vcmpbfp_p");
   pragma Import (LL_Altivec, vcmpeqfp_p,  "__builtin_altivec_vcmpeqfp_p");
   pragma Import (LL_Altivec, vcmpgefp_p,  "__builtin_altivec_vcmpgefp_p");
   pragma Import (LL_Altivec, vcmpgtfp_p,  "__builtin_altivec_vcmpgtfp_p");
   pragma Import (LL_Altivec, vcmpequw_p,  "__builtin_altivec_vcmpequw_p");
   pragma Import (LL_Altivec, vcmpgtsw_p,  "__builtin_altivec_vcmpgtsw_p");
   pragma Import (LL_Altivec, vcmpgtuw_p,  "__builtin_altivec_vcmpgtuw_p");
   pragma Import (LL_Altivec, vcmpgtuh_p,  "__builtin_altivec_vcmpgtuh_p");
   pragma Import (LL_Altivec, vcmpgtsh_p,  "__builtin_altivec_vcmpgtsh_p");
   pragma Import (LL_Altivec, vcmpequh_p,  "__builtin_altivec_vcmpequh_p");
   pragma Import (LL_Altivec, vcmpequb_p,  "__builtin_altivec_vcmpequb_p");
   pragma Import (LL_Altivec, vcmpgtsb_p,  "__builtin_altivec_vcmpgtsb_p");
   pragma Import (LL_Altivec, vcmpgtub_p,  "__builtin_altivec_vcmpgtub_p");

end GNAT.Altivec.Low_Level_Vectors;
