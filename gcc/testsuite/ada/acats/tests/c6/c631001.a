-- C631001.A
--
--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained 
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making 
--     this public release, the Government intends to confer upon all 
--     recipients unlimited rights  equal to those held by the Government.  
--     These rights include rights to use, duplicate, release or disclose the 
--     released technical data and computer software in whole or in part, in 
--     any manner and for any purpose whatsoever, and to have or permit others 
--     to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED 
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE 
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
--
-- OBJECTIVE:
--      Check that if different forms of a name are used in the default 
--      expression of a discriminant part, the selector may be an operator
--      symbol or a character literal.
--
-- TEST DESCRIPTION:
--      This transition test defines private types where their selectors in
--      the default expression of the discriminant parts at the full type 
--      declarations are an operator and a literal, respectively. 
--      The test also declares procedures that use an operator and a literal
--      as selectors in the formal parts. 
--
--      Inspired by B63102A.ADA.
--
--
-- CHANGE HISTORY:
--      25 Mar 96   SAIC    Initial version for ACVC 2.1.
--      26 Feb 97   PWB.CTA Removed use of function called before elaboration
--!

with Report;

procedure C631001 is

   package C631001_0 is 

      type Int_Type is range 1 .. 100;
      type Enu_Type is ('A', 'B', 'C', 'D');

      type Private_Enu (D : Enu_Type := 'B')       is private;

      function "+" (X, Y : Int_Type) return Int_Type;

      procedure Int_Proc (P1 : in  Int_Type := "+" (10, 15);
                          P2 : out Int_Type);
                                    
      procedure Enu_Proc (P1 : in  Enu_Type := 'C';
                          P2 : out Enu_Type);

   private

      type Private_Enu (D : Enu_Type := C631001_0.'B') is              -- OK.
        record
           C2 : Enu_Type := D;
        end record;

      -----------------------------------------------------------------
      PE_Obj  : C631001_0.Private_Enu;

   end C631001_0;

     --==================================================================--

   package body C631001_0 is 

      function "+" (X, Y : Int_Type) return Int_Type is
      begin
         return 10;
      end "+";

      -----------------------------------------------------------------
      procedure Int_Proc (P1 : in  Int_Type := C631001_0."+" (10, 15); -- OK.
                          P2 : out Int_Type) is
                                    
      begin
         P2 := P1;
      end Int_Proc;

      -----------------------------------------------------------------
      procedure Enu_Proc (P1 : in  Enu_Type := C631001_0.'C';          -- OK.
                          P2 : out Enu_Type) is
      begin
         P2 := P1;
      end Enu_Proc;

      -----------------------------------------------------------------

   end C631001_0;

   ---------------------------------------------------------------------------
   Int_Obj : C631001_0.Int_Type := 50;
   Enu_Obj : C631001_0.Enu_Type := C631001_0.'D';

   -- Direct visibility to operator symbols
   use type C631001_0.Int_Type;
   use type C631001_0.Enu_Type;

begin  -- main

   Report.Test ("C631001", "Check that if different forms of a name are " &
                "used in the default expression of a discriminant part, " &
                "the selector may be an operator symbol or a character "  &
                "literal");

   C631001_0.Int_Proc (P2 => Int_Obj);

   if Int_Obj /= 10 then
     Report.Failed ("Wrong result for Int_Obj");
   end if;

   C631001_0.Enu_Proc (P2 => Enu_Obj);

   if Enu_Obj /= 'C' then
     Report.Failed ("Wrong result for Enu_Obj");
   end if;

   Report.Result;

end C631001;
