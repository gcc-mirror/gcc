-- C371001.A
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
--      Check that if a discriminant constraint depends on a discriminant,
--      the evaluation of the expressions in the constraint is deferred 
--      until an object of the subtype is created.  Check for cases of 
--      records with private type component.
--
-- TEST DESCRIPTION:
--      This transition test defines record type and incomplete types with 
--      discriminant components which depend on the discriminants.  The 
--      discriminants are calculated by function calls.  The test verifies
--      that Constraint_Error is raised during the object creations when 
--      values of discriminants are incompatible with the subtypes.
--
--      Inspired by C37214A.ADA and C37216A.ADA.
--
--
-- CHANGE HISTORY:
--      11 Apr 96   SAIC    Initial version for ACVC 2.1.
--      06 Oct 96   SAIC    Added LM references. Replaced "others exception"
--                          with "unexpected exception"                     
--
--!

with Report; 

procedure C371001 is

   subtype Small_Int is Integer range 1..10;

   Func1_Cons : Integer := 0;

   ---------------------------------------------------------
   function Func1 return Integer is
   begin
      Func1_Cons := Func1_Cons + Report.Ident_Int(1);
      return Func1_Cons;
   end Func1;


begin
   Report.Test ("C371001", "Check that if a discriminant constraint " &
                "depends on a discriminant, the evaluation of the "   &
                "expressions in the constraint is deferred until "    &
                "object declarations");

   ---------------------------------------------------------
   -- Constraint checks on an object declaration of a record.

   begin

      declare

         package C371001_0 is

            type PT_W_Disc (D : Small_Int) is private;
            type Rec_W_Private (D1 : Integer) is
              record
                 C : PT_W_Disc (D1);
              end record;
   
            type Rec (D3 : Integer) is
              record
                 C1 : Rec_W_Private (D3);
              end record;

         private
            type PT_W_Disc (D : Small_Int) is 
              record
                 Str : String (1 .. D) := (others => '*');
              end record;

         end C371001_0;                         

         --=====================================================--

         Obj : C371001_0.Rec(Report.Ident_Int(0));  -- Constraint_Error raised.

      begin
         Report.Failed ("Obj - Constraint_Error should be raised");
         if Obj.C1.D1 /= 0 then
            Report.Failed ("Obj - Shouldn't get here");
         end if;

      exception
         when others           => 
              Report.Failed ("Obj - exception raised too late");
      end;

   exception                      
      when Constraint_Error =>                      -- Exception expected.
           null;
      when others           => 
           Report.Failed ("Obj - unexpected exception raised");
   end;

   -------------------------------------------------------------------
   -- Constraint checks on an object declaration of an array.

   begin
      declare

         package C371001_1 is

            type PT_W_Disc (D : Small_Int) is private;
            type Rec_W_Private (D1 : Integer) is
              record
                 C : PT_W_Disc (D1);
              end record;

            type Rec_01 (D3 : Integer) is
              record
                 C1 : Rec_W_Private (D3);
              end record;

            type Arr is array (1 .. 5) of 
              Rec_01(Report.Ident_Int(0));          -- No Constraint_Error 
                                                    -- raised. 
         private
            type PT_W_Disc (D : Small_Int) is 
              record
                 Str : String (1 .. D) := (others => '*');
              end record;

         end C371001_1;                         

         --=====================================================--

      begin
         declare
            Obj1 : C371001_1.Arr;                   -- Constraint_Error raised.
         begin
            Report.Failed ("Obj1 - Constraint_Error should be raised");
            if Obj1(1).D3 /= 0 then
               Report.Failed ("Obj1 - Shouldn't get here");
            end if;

         exception
            when others           => 
                 Report.Failed ("Obj1 - exception raised too late");
         end;

      exception
         when Constraint_Error =>                   -- Exception expected.
              null;
         when others =>
              Report.Failed ("Obj1 - unexpected exception raised");
      end;

   exception
      when Constraint_Error =>
           Report.Failed ("Arr - Constraint_Error raised");
      when others =>
           Report.Failed ("Arr - unexpected exception raised");
   end;


   -------------------------------------------------------------------
   -- Constraint checks on an object declaration of an access type.

   begin
      declare

         package C371001_2 is

            type PT_W_Disc (D : Small_Int) is private;
            type Rec_W_Private (D1 : Integer) is
              record
                 C : PT_W_Disc (D1);
              end record;
   
            type Rec_02 (D3 : Integer) is
              record
                 C1 : Rec_W_Private (D3);
              end record;

            type Acc_Rec2 is access Rec_02          -- No Constraint_Error 
              (Report.Ident_Int(11));               -- raised. 

         private
            type PT_W_Disc (D : Small_Int) is 
              record
                 Str : String (1 .. D) := (others => '*');
              end record;

         end C371001_2;                         

         --=====================================================--

      begin
         declare
            Obj2 : C371001_2.Acc_Rec2;              -- No Constraint_Error 
                                                    -- raised.
         begin
            Obj2 := new C371001_2.Rec_02 (Report.Ident_Int(11));       
                                                    -- Constraint_Error raised.

            Report.Failed ("Obj2 - Constraint_Error should be raised");
            if Obj2.D3 /= 1 then
               Report.Failed ("Obj2 - Shouldn't get here");
            end if;

         exception
            when Constraint_Error =>                -- Exception expected.
               null;
            when others           => 
               Report.Failed ("Obj2 - unexpected exception raised in " &
                              "assignment");
         end;

      exception
         when Constraint_Error =>  
              Report.Failed ("Obj2 - Constraint_Error raised in declaration");
         when others =>
              Report.Failed ("Obj2 - unexpected exception raised in " &
                             "declaration");
      end;

   exception
      when Constraint_Error =>  
           Report.Failed ("Acc_Rec2 - Constraint_Error raised");
      when others =>
           Report.Failed ("Acc_Rec2 - unexpected exception raised");
   end;

   -------------------------------------------------------------------
   -- Constraint checks on an object declaration of a subtype.

   Func1_Cons := -1;

   begin
      declare

         package C371001_3 is

            type PT_W_Disc (D1, D2 : Small_Int) is private;
            type Rec_W_Private (D3, D4 : Integer) is
              record
                 C : PT_W_Disc (D3, D4);
              end record;
   
            type Rec_03 (D5 : Integer) is
              record
                 C1 : Rec_W_Private (D5, Func1);     -- Func1 evaluated,
              end record;                            -- value 0.

            subtype Subtype_Rec is Rec_03(1);        -- No Constraint_Error 
                                                     -- raised. 
         private
            type PT_W_Disc (D1, D2 : Small_Int) is 
              record
                 Str1 : String (1 .. D1) := (others => '*');
                 Str2 : String (1 .. D2) := (others => '*');
              end record;

         end C371001_3;                         

         --=====================================================--

      begin
         declare
            Obj3 : C371001_3.Subtype_Rec;            -- Constraint_Error raised.
         begin
            Report.Failed ("Obj3 - Constraint_Error should be raised");
            if Obj3.D5 /= 1 then
               Report.Failed ("Obj3 - Shouldn't get here");
            end if;

         exception
            when others           => 
                 Report.Failed ("Obj3 - exception raised too late");
         end;

      exception
         when Constraint_Error =>                    -- Exception expected.
              null;
         when others =>
              Report.Failed ("Obj3 - unexpected exception raised");
      end;

   exception
      when Constraint_Error =>
           Report.Failed ("Subtype_Rec - Constraint_Error raised");
      when others =>
           Report.Failed ("Subtype_Rec - unexpected exception raised");
   end;

   -------------------------------------------------------------------
   -- Constraint checks on an object declaration of an incomplete type.

   Func1_Cons := 10;

   begin
      declare

         package C371001_4 is

            type Rec_04 (D3 : Integer);
            type PT_W_Disc (D : Small_Int) is private;
            type Rec_W_Private (D1, D2 : Small_Int) is
              record
                 C : PT_W_Disc (D2);
              end record;

            type Rec_04 (D3 : Integer) is
              record
                 C1 : Rec_W_Private (D3, Func1);     -- Func1 evaluated
              end record;                            -- value 11.

            type Acc_Rec4 is access Rec_04 (1);      -- No Constraint_Error 
                                                     -- raised. 
         private
            type PT_W_Disc (D : Small_Int) is 
              record
                 Str : String (1 .. D) := (others => '*');
              end record;

         end C371001_4;                         

         --=====================================================--

      begin
         declare
            Obj4 : C371001_4.Acc_Rec4;               -- No Constraint_Error 
                                                     -- raised.
         begin
            Obj4 := new C371001_4.Rec_04 (1);        -- Constraint_Error raised.

            Report.Failed ("Obj4 - Constraint_Error should be raised");
            if Obj4.D3 /= 1 then
               Report.Failed ("Obj4 - Shouldn't get here");
            end if;

         exception
            when Constraint_Error =>                 -- Exception expected.
               null;
            when others           => 
               Report.Failed ("Obj4 - unexpected exception raised in " &
                              "assignment");
         end;

      exception
         when Constraint_Error =>  
              Report.Failed ("Obj4 - Constraint_Error raised in declaration");
         when others =>
              Report.Failed ("Obj4 - unexpected exception raised in " &
                             "declaration");
      end;

   exception
      when Constraint_Error =>  
           Report.Failed ("Acc_Rec4 - Constraint_Error raised");
      when others =>
           Report.Failed ("Acc_Rec4 - unexpected exception raised");
   end;

   Report.Result;

exception
   when others =>
        Report.Failed ("Discriminant value checked too soon");
        Report.Result;

end C371001;
