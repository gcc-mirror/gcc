-- C460009.A
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
--     Check that Constraint_Error is raised in cases of null arrays when:
--     1.  an assignment is made to a null array if the length of each
--         dimension of the operand does not match the length of 
--         the corresponding dimension of the target subtype.
--     2.  an array actual parameter does not match the length of
--         corresponding dimensions of the formal in out parameter where
--         the actual parameter has the form of a type conversion.     
--     3.  an array actual parameter does not match the length of
--         corresponding dimensions of the formal out parameter where
--         the actual parameter has the form of a type conversion.     
--
-- TEST DESCRIPTION:
--      This transition test creates examples where array of null ranges 
--      raises Constraint_Error if any of the lengths mismatch.
--
--      Inspired by C52103S.ADA, C64105E.ADA, and C64105F.ADA.
--
--
-- CHANGE HISTORY:
--      21 Mar 96   SAIC    Initial version for ACVC 2.1.
--      21 Sep 96   SAIC    ACVC 2.1: Added new case.
--
--!

with Report;

procedure C460009 is

   subtype Int is Integer range 1 .. 3;

begin

   Report.Test("C460009","Check that Constraint_Error is raised in "  &
               "cases of null arrays if any of the lengths mismatch " & 
               "in assignments and parameter passing");

   ---------------------------------------------------------------------------
   declare

      type Arr_Int1 is array (Int range <>) of Integer;
      Arr_Obj1 : Arr_Int1 (2 .. Report.Ident_Int(1));     -- null array object

   begin

      -- Same lengths, no Constraint_Error raised.
      Arr_Obj1 := (Report.Ident_Int(3) .. 2 => Report.Ident_Int(1));

      Report.Comment ("Dead assignment prevention in Arr_Obj1 => " &
                       Integer'Image (Arr_Obj1'Last));

   exception

      when Constraint_Error => 
        Report.Failed ("Arr_Obj1 - Constraint_Error exception raised");
      when others           => 
        Report.Failed ("Arr_Obj1 - others exception raised");

   end;

   ---------------------------------------------------------------------------
   declare

      type Arr_Int2 is array (Int range <>, Int range <>) of Integer;
      Arr_Obj2 : Arr_Int2 (1 .. Report.Ident_Int(2), 
                           Report.Ident_Int(3) .. Report.Ident_Int(2));
                                                           -- null array object
   begin

      -- Same lengths, no Constraint_Error raised.
      Arr_Obj2 := Arr_Int2'(Report.Ident_Int(2) .. 3 => 
                  (Report.Ident_Int(2) .. Report.Ident_Int(1) =>  
                   Report.Ident_Int(1)));

      Report.Comment ("Dead assignment prevention in Arr_Obj2 => " &
                       Integer'Image (Arr_Obj2'Last));

   exception

      when Constraint_Error => 
        Report.Failed ("Arr_Obj2 - Constraint_Error exception raised");
      when others           => 
        Report.Failed ("Arr_Obj2 - others exception raised");

   end;
   
   ---------------------------------------------------------------------------
   declare

      type Arr_Int3 is array (Int range <>, Int range <>) of Integer;
      Arr_Obj3 : Arr_Int3 (1 .. Report.Ident_Int(2), 
                           Report.Ident_Int(3) .. Report.Ident_Int(2));
                                                           -- null array object

   begin

      -- Lengths mismatch, Constraint_Error raised.
      Arr_Obj3 := Arr_Int3'(Report.Ident_Int(3) .. 2 => 
                  (Report.Ident_Int(1) .. Report.Ident_Int(3) =>  
                   Report.Ident_Int(1)));

      Report.Comment ("Dead assignment prevention in Arr_Obj3 => " &
                       Integer'Image (Arr_Obj3'Last));

      Report.Failed ("Constraint_Error not raised in Arr_Obj3");

   exception

      when Constraint_Error => null;      -- exception expected.
      when others           => 
        Report.Failed ("Arr_Obj3 - others exception raised");

   end;

   ---------------------------------------------------------------------------
   declare

      type Arr_Int4 is array (Int range <>, Int range <>, Int range <>) of 
        Integer;
      Arr_Obj4 : Arr_Int4 (1 .. Report.Ident_Int(2), 
                           Report.Ident_Int(1) .. Report.Ident_Int(3),
                           Report.Ident_Int(3) .. Report.Ident_Int(2));
                                                           -- null array object
   begin

      -- Lengths mismatch, Constraint_Error raised.
      Arr_Obj4 := Arr_Int4'(Report.Ident_Int(1) .. 3 => 
                  (Report.Ident_Int(1) .. Report.Ident_Int(2) =>  
                   (Report.Ident_Int(3) .. Report.Ident_Int(2) =>  
                   Report.Ident_Int(1))));

      Report.Comment ("Dead assignment prevention in Arr_Obj4 => " &
                       Integer'Image (Arr_Obj4'Last));

      Report.Failed ("Constraint_Error not raised in Arr_Obj4");

   exception

      when Constraint_Error => null;      -- exception expected.
      when others           => 
        Report.Failed ("Arr_Obj4 - others exception raised");

   end;

   ---------------------------------------------------------------------------
   declare

      type Arr_Int5 is array (Int range <>) of Integer;
      Arr_Obj5 : Arr_Int5 (2 .. Report.Ident_Int(1));     -- null array object

   begin

      -- Only lengths of two null ranges are different, no Constraint_Error 
      -- raised.
      Arr_Obj5 := (Report.Ident_Int(3) .. 1 => Report.Ident_Int(1));

      Report.Comment ("Dead assignment prevention in Arr_Obj5 => " &
                       Integer'Image (Arr_Obj5'Last));

   exception

      when Constraint_Error => 
        Report.Failed ("Arr_Obj5 - Constraint_Error exception raised");
      when others           => 
        Report.Failed ("Arr_Obj5 - others exception raised");

   end;

   ---------------------------------------------------------------------------
   declare
      subtype Str is String (Report.Ident_Int(5) .. 4);  
                                                            -- null string
      Str_Obj : Str;

   begin

      -- Same lengths, no Constraint_Error raised.
      Str_Obj := (Report.Ident_Int(1) .. 0 => 'Z');
      Str_Obj(2 .. 1) := "";
      Str_Obj(4 .. 2) := (others => 'X');
      Str_Obj(Report.Ident_Int(6) .. 3) := "";
      Str_Obj(Report.Ident_Int(0) .. Report.Ident_Int(-1)) := (others => 'Y');

   exception

      when Constraint_Error => 
        Report.Failed ("Str_Obj - Constraint_Error exception raised");
      when others           => 
        Report.Failed ("Str_Obj - others exception raised");

   end;

   ---------------------------------------------------------------------------
   declare

      type Arr_Char5 is array (Int range <>, Int range <>) of Character;
      subtype Formal is Arr_Char5 
        (Report.Ident_Int(2) .. 0, 1 .. Report.Ident_Int(3));  
      Arr_Obj5 : Arr_Char5 (Report.Ident_Int(2) .. Report.Ident_Int(1),
                            Report.Ident_Int(1) .. Report.Ident_Int(2))
               := (Report.Ident_Int(2) .. Report.Ident_Int(1) =>
                  (Report.Ident_Int(1) .. Report.Ident_Int(2) => ' ')); 

      procedure Proc5 (P : in out Formal) is
      begin
         Report.Failed ("No exception raised in Proc5");

      exception       

         when Constraint_Error => 
           Report.Failed ("Constraint_Error exception raised in Proc5");
         when others           => 
           Report.Failed ("Others exception raised in Proc5");
      end;            

   begin

      -- Lengths mismatch in the type conversion, Constraint_Error raised.
      Proc5 (Formal(Arr_Obj5));

      Report.Failed ("Constraint_Error not raised in the call Proc5");

   exception

      when Constraint_Error => null;      -- exception expected.
      when others           => 
        Report.Failed ("Arr_Obj5 - others exception raised");

   end;

   ---------------------------------------------------------------------------
   declare

      type Formal is array 
        (Report.Ident_Int(1) .. 3, 3 .. Report.Ident_Int(1)) of Character;

      type Actual is array 
        (Report.Ident_Int(5) .. 3, 3 .. Report.Ident_Int(5)) of Character;

      Arr_Obj6 : Actual := (5 .. 3 => (3 .. 5 => ' '));

      procedure Proc6 (P : in out Formal) is
      begin
         Report.Failed ("No exception raised in Proc6");

      exception       

         when Constraint_Error => 
           Report.Failed ("Constraint_Error exception raised in Proc6");
         when others           => 
           Report.Failed ("Others exception raised in Proc6");
      end;            

   begin

      -- Lengths mismatch in the type conversion, Constraint_Error raised.
      Proc6 (Formal(Arr_Obj6));

      Report.Failed ("Constraint_Error not raised in the call Proc6");

   exception

      when Constraint_Error => null;      -- exception expected.
      when others           => 
        Report.Failed ("Arr_Obj6 - others exception raised");

   end;

   ---------------------------------------------------------------------------
   declare

      type Formal is array (Int range <>, Int range <>) of Character;
      type Actual is array (Positive range 5 .. 2,
                            Positive range 1 .. 3) of Character;

      Arr_Obj7 : Actual := (5 .. 2 => (1 .. 3 => ' '));

      procedure Proc7 (P : in out Formal) is
      begin
         if P'Last /= 2 and P'Last(2) /= 3 then
            Report.Failed ("Wrong bounds passed for Arr_Obj7");
         end if;

         -- Lengths mismatch, Constraint_Error raised.
         P := (1 .. 3 => (3 .. 0 => ' '));

         Report.Comment ("Dead assignment prevention in Proc7 => " &
                          Integer'Image (P'Last));

         Report.Failed ("No exception raised in Proc7");

      exception       

         when Constraint_Error => null;      -- exception expected.
         when others           => 
           Report.Failed ("Others exception raised in Proc7");
      end;            

   begin

      -- Same lengths, no Constraint_Error raised.
      Proc7 (Formal(Arr_Obj7));

      if Arr_Obj7'Last /= 2 and Arr_Obj7'Last(2) /= 3 then
         Report.Failed ("Bounds changed for Arr_Obj7");
      end if;

   exception

      when Constraint_Error => 
        Report.Failed ("Constraint_Error exception raised after call Proc7");
      when others           => 
        Report.Failed ("Arr_Obj7 - others exception raised");

   end;

   ---------------------------------------------------------------------------
   declare

      type Arr_Char8 is array (Int range <>, Int range <>) of Character;
      subtype Formal is Arr_Char8 
        (Report.Ident_Int(2) .. 0, 1 .. Report.Ident_Int(3));  
      Arr_Obj8 : Arr_Char8 (Report.Ident_Int(2) .. Report.Ident_Int(1),
                            Report.Ident_Int(1) .. Report.Ident_Int(2));

      procedure Proc8 (P : out Formal) is
      begin
         Report.Failed ("No exception raised in Proc8");

      exception       

         when Constraint_Error => 
           Report.Failed ("Constraint_Error exception raised in Proc8");
         when others           => 
           Report.Failed ("Others exception raised in Proc8");
      end;            

   begin

      -- Lengths mismatch in the type conversion, Constraint_Error raised.
      Proc8 (Formal(Arr_Obj8));

      Report.Failed ("Constraint_Error not raised in the call Proc8");

   exception

      when Constraint_Error => null;      -- exception expected.
      when others           => 
        Report.Failed ("Arr_Obj8 - others exception raised");

   end;

   ---------------------------------------------------------------------------
   declare

      type Formal is array 
        (Report.Ident_Int(1) .. 3, 3 .. Report.Ident_Int(1)) of Character;

      type Actual is array 
        (Report.Ident_Int(5) .. 3, 3 .. Report.Ident_Int(5)) of Character;

      Arr_Obj9 : Actual;

      procedure Proc9 (P : out Formal) is
      begin
         Report.Failed ("No exception raised in Proc9");

      exception       

         when Constraint_Error => 
           Report.Failed ("Constraint_Error exception raised in Proc9");
         when others           => 
           Report.Failed ("Others exception raised in Proc9");
      end;            

   begin

      -- Lengths mismatch in the type conversion, Constraint_Error raised.
      Proc9 (Formal(Arr_Obj9));

      Report.Failed ("Constraint_Error not raised in the call Proc9");

   exception

      when Constraint_Error => null;      -- exception expected.
      when others           => 
        Report.Failed ("Arr_Obj9 - others exception raised");

   end;

   ---------------------------------------------------------------------------
   declare

      type Formal is array (Int range <>, Int range <>) of Character;
      type Actual is array (Positive range 5 .. 2,
                            Positive range 1 .. 3) of Character;

      Arr_Obj10 : Actual;

      procedure Proc10 (P : out Formal) is
      begin
         if P'Last /= 2 and P'Last(2) /= 3 then
            Report.Failed ("Wrong bounds passed for Arr_Obj10");
         end if;

         -- Lengths mismatch, Constraint_Error raised.
         P := (1 .. 3 => (3 .. 1 => ' '));

         Report.Comment ("Dead assignment prevention in Proc10 => " &
                          Integer'Image (P'Last));

         Report.Failed ("No exception raised in Proc10");

      exception       

         when Constraint_Error => null;      -- exception expected.
         when others           => 
           Report.Failed ("Others exception raised in Proc10");
      end;            

   begin

      -- Same lengths, no Constraint_Error raised.
      Proc10 (Formal(Arr_Obj10));

      if Arr_Obj10'Last /= 2 and Arr_Obj10'Last(2) /= 3 then
         Report.Failed ("Bounds changed for Arr_Obj10");
      end if;

   exception

      when Constraint_Error => 
        Report.Failed ("Constraint_Error exception raised after call Proc10");
      when others           => 
        Report.Failed ("Arr_Obj10 - others exception raised");

   end;

   ---------------------------------------------------------------------------
   Report.Result;

end C460009;
