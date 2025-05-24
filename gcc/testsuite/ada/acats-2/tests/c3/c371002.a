-- C371002.A
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
--      the evaluation of the expressions in the constraint is deferred until
--      an object of the subtype is created.  Check for cases of records.
--
-- TEST DESCRIPTION:
--      This transition test defines record types with discriminant components
--      which depend on the discriminants.  The discriminants are calculated 
--      by function calls.  The test verifies that Constraint_Error is raised 
--      during the object creations when values of discriminants are 
--      incompatible with the subtypes.
--
--      Inspired by C37213A.ADA, C37213C.ADA, C37215A.ADA and C37215C.ADA.
--
--
-- CHANGE HISTORY:
--      05 Apr 96   SAIC    Initial version for ACVC 2.1.
--
--!

with Report; 

procedure C371002 is

   subtype Small_Int is Integer range 1..10;

   type Rec_W_Disc (Disc1, Disc2 : Small_Int) is 
     record 
        Str1 : String (1 .. Disc1) := (others => '*');
        Str2 : String (1 .. Disc2) := (others => '*');
     end record;

   type My_Array is array (Small_Int range <>) of Integer;

   Func1_Cons : Integer := 0;

   ---------------------------------------------------------
   function Chk (Cons    : Integer;
                 Value   : Integer;
                 Message : String) return Boolean is
   begin
      if Cons /= Value then
         Report.Failed (Message & ": Func1_Cons is " &
                        Integer'Image(Func1_Cons));
      end if;
      return True;
   end Chk;

   ---------------------------------------------------------
   function Func1 return Integer is
   begin
      Func1_Cons := Func1_Cons + Report.Ident_Int(1);
      return Func1_Cons;
   end Func1;

begin
   Report.Test ("C371002", "Check that if a discriminant constraint " &
                "depends on a discriminant, the evaluation of the "   &
                "expressions in the constraint is deferred until "    &
                "object declarations");

   ---------------------------------------------------------
   declare
      type Rec1 (D3 : Integer) is
        record
           C1 : Rec_W_Disc (D3, Func1);      -- Func1 evaluated, value 1.
        end record;
      
      Chk1 : Boolean := Chk (Func1_Cons, 1,
                             "Func1 not evaluated for Rec1");

      Obj1 : Rec1 (1);                       -- Func1 not evaluated again.
      Obj2 : Rec1 (2);                       -- Func1 not evaluated again. 

      Chk2 : Boolean := Chk (Func1_Cons, 1,
                             "Func1 evaluated too many times");
   begin
      if Obj1 /= (D3 => 1, 
                  C1 => (Disc1   => 1,  
                         Disc2   => 1, 
                         Str1    => (others => '*'),
                         Str2    => (others => '*'))) or
         Obj2 /= (D3 => 2, 
                  C1 => (Disc1   => 2,  
                         Disc2   => 1, 
                         Str1    => (others => '*'),
                         Str2    => (others => '*'))) then
           Report.Failed ("Obj1 & Obj2 - Discriminant values not correct");
      end if;
   end;

   ---------------------------------------------------------
   Func1_Cons := -11;

   declare
      type Rec_Of_Rec_01 (D3 : Integer) is
        record
           C1 : Rec_W_Disc (D3, Func1);      -- Func1 evaluated, value -10.
        end record;                          -- Constraint_Error not raised. 

      type Rec_Of_MyArr_01 (D3 : Integer) is
        record
           C1 : My_Array (Func1 .. D3);      -- Func1 evaluated, value -9.
        end record;                          -- Constraint_Error not raised. 

      type Rec_Of_Rec_02 (D3 : Integer) is
        record
           C1 : Rec_W_Disc (D3, 1);
        end record;

      type Rec_Of_MyArr_02 (D3 : Integer) is
        record
           C1 : My_Array (D3 .. 1);       
        end record;

   begin

      ---------------------------------------------------------
      begin
         declare
            Obj3 : Rec_Of_Rec_01(1);         -- Constraint_Error raised.
         begin
            Report.Failed ("Obj3 - Constraint_Error should be raised");
            if Obj3 /= (1, (1, 1, others => (others => '*'))) then
               Report.Comment ("Obj3 - Shouldn't get here");
            end if;
         end;

      exception
         when Constraint_Error =>            -- Exception expected.
              null;
         when others           => 
              Report.Failed ("Obj3 - others exception raised");
      end;
        
      ---------------------------------------------------------
      begin
         declare
            subtype Subtype_Rec is Rec_Of_Rec_01(1);  
                                             -- No Constraint_Error raised.
         begin
            declare
               Obj4 : Subtype_Rec;           -- Constraint_Error raised.
            begin
               Report.Failed ("Obj4 - Constraint_Error should be raised");
               if Obj4 /= (D3 => 1, 
                           C1 => (Disc1   => 1,  
                                  Disc2   => 1, 
                                  Str1    => (others => '*'),
                                  Str2    => (others => '*'))) then
                  Report.Comment ("Obj4 - Shouldn't get here");
               end if;
            end;

         exception
            when Constraint_Error =>         -- Exception expected.
                null;
            when others =>
                Report.Failed ("Obj4 - others exception raised");
         end;

      exception
         when Constraint_Error =>
              Report.Failed ("Subtype_Rec - Constraint_Error raised");
         when others =>
              Report.Failed ("Subtype_Rec - others exception raised");
      end;

      ---------------------------------------------------------
      begin
         declare
            type Arr is array (1..5)         -- No Constraint_Error raised.
              of Rec_Of_Rec_01(1);                         
                                                       
         begin
            declare
               Obj5 : Arr;                   -- Constraint_Error raised.
            begin
               Report.Failed ("Obj5 - Constraint_Error should be raised");
               if Obj5 /= (1..5 => (1, (1, 1, others => (others => '*')))) then
                  Report.Comment ("Obj5 - Shouldn't get here");
               end if;
            end;

         exception
            when Constraint_Error =>         -- Exception expected.
                null;
            when others =>
                Report.Failed ("Obj5 - others exception raised");
         end;

      exception
         when Constraint_Error =>
              Report.Failed ("Arr - Constraint_Error raised");
         when others =>
              Report.Failed ("Arr - others exception raised");
      end;

      ---------------------------------------------------------
      begin
         declare
            type Rec_Of_Rec_Of_MyArr is
              record
                 C1 : Rec_Of_MyArr_01(1);    -- No Constraint_Error raised.
              end record;                    
         begin
            declare
               Obj6 : Rec_Of_Rec_Of_MyArr;   -- Constraint_Error raised.
            begin
               Report.Failed ("Obj6 - Constraint_Error should be raised");
               if Obj6 /= (C1 => (1, (1, 1))) then
                  Report.Comment ("Obj6 - Shouldn't get here");
               end if;
            end;

         exception
            when Constraint_Error =>         -- Exception expected.
                null;
            when others =>
                Report.Failed ("Obj6 - others exception raised");
         end;

      exception
         when Constraint_Error =>
              Report.Failed ("Rec_Of_Rec_Of_MyArr - Constraint_Error raised");
         when others =>
              Report.Failed ("Rec_Of_Rec_Of_MyArr - others exception raised");
      end;

      ---------------------------------------------------------
      begin
         declare
            type New_Rec is 
              new Rec_Of_MyArr_01(1);        -- No Constraint_Error raised.

         begin
            declare
               Obj7 : New_Rec;               -- Constraint_Error raised.
            begin
               Report.Failed ("Obj7 - Constraint_Error should be raised");
               if Obj7 /= (1, (1, 1)) then
                  Report.Comment ("Obj7 - Shouldn't get here");
               end if;
            end;

         exception
            when Constraint_Error =>         -- Exception expected.
                null;
            when others =>
                Report.Failed ("Obj7 - others exception raised");
         end;

      exception
         when Constraint_Error =>
              Report.Failed ("New_Rec - Constraint_Error raised");
         when others =>
              Report.Failed ("New_Rec - others exception raised");
      end;

      ---------------------------------------------------------
      begin
         declare
            type Acc_Rec is 
              access Rec_Of_Rec_02 (Report.Ident_Int(0));
                                             -- No Constraint_Error raised.
         begin
            declare
               Obj8 : Acc_Rec;               -- No Constraint_Error raised.

            begin                                   
               Obj8 := new Rec_Of_Rec_02 (Report.Ident_Int(0));
                                             -- Constraint_Error raised.

               Report.Failed ("Obj8 - Constraint_Error should be raised");
               if Obj8.all /= (D3 => 1, 
                               C1 => (Disc1   => 1,  
                                      Disc2   => 1, 
                                      Str1    => (others => '*'),
                                      Str2    => (others => '*'))) then
                  Report.Comment ("Obj8 - Shouldn't get here");
               end if;
            end;

         exception
            when Constraint_Error =>         -- Exception expected.
                null;
            when others =>
                Report.Failed ("Obj8 - others exception raised");
         end;

      exception
         when Constraint_Error =>
              Report.Failed ("Acc_Rec - Constraint_Error raised");
         when others =>
              Report.Failed ("Acc_Rec - others exception raised");
      end;

      ---------------------------------------------------------
      begin
         declare
            type Acc_Rec_MyArr is access 
              Rec_Of_MyArr_02;               -- No Constraint_Error 
                                             -- raised for either
            Obj9 : Acc_Rec_MyArr;            -- declaration.

         begin
            Obj9 := new Rec_Of_MyArr_02 (Report.Ident_Int(0));
                                             -- Constraint_Error raised.

            Report.Failed ("Obj9 - Constraint_Error should be raised");

            if Obj9.all /= (1, (1, 1)) then
               Report.Comment ("Obj9 - Shouldn't get here");
            end if;
       
         exception
            when Constraint_Error =>         -- Exception expected.
                null;
            when others =>
                Report.Failed ("Obj9 - others exception raised");
         end;

      exception
         when Constraint_Error =>
              Report.Failed ("Acc_Rec_MyArr - Constraint_Error raised");
         when others =>
              Report.Failed ("Acc_Rec_MyArr - others exception raised");
      end;

   end;      

   Report.Result;

exception
     when others =>
          Report.Failed ("Discriminant value checked too soon");
          Report.Result;

end C371002;
