-- C360002.A
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
--      Check that modular types may be used as array indices.
--
--      Check that if aliased appears in the component_definition of an
--      array_type that each component of the array is aliased.
--
--      Check that references to aliased array objects produce correct
--      results, and that out-of-bounds indexing correctly produces
--      Constraint_Error.
--
-- TEST DESCRIPTION:
--      This test defines several array types and subtypes indexed by modular
--      types; some aliased some not, some with aliased components, some not.
--
--      It then checks that assignments move the correct data.
--
--
-- CHANGE HISTORY:
--      28 SEP 95   SAIC   Initial version
--      23 APR 96   SAIC   Doc fixes, fixed constrained/unconstrained conflict
--      13 FEB 97   PWB.CTA Removed illegal declarations and affected code
--!

------------------------------------------------------------------- C360002

with Report;

procedure C360002 is

  Verbose : Boolean := Report.Ident_Bool( False );

  type Mod_128 is mod 128;

  function Ident_128( I: Integer ) return Mod_128 is
  begin
    return Mod_128( Report.Ident_Int( I ) );
  end Ident_128;

  type Unconstrained_Array
       is array( Mod_128 range <> ) of Integer;

  type Unconstrained_Array_Aliased
       is array( Mod_128 range <> ) of aliased Integer;

  type Access_All_Unconstrained_Array
       is access all Unconstrained_Array;

  type Access_All_Unconstrained_Array_Aliased
       is access all Unconstrained_Array_Aliased;

  subtype Array_01_10
          is Unconstrained_Array(01..10);

  subtype Array_11_20
          is Unconstrained_Array(11..20);

  subtype Array_Aliased_01_10
          is Unconstrained_Array_Aliased(01..10);

  subtype Array_Aliased_11_20
          is Unconstrained_Array_Aliased(11..20);

  subtype Access_All_01_10_Array
          is Access_All_Unconstrained_Array(01..10);

  subtype Access_All_01_10_Array_Aliased
          is Access_All_Unconstrained_Array_Aliased(01..10);

  subtype Access_All_11_20_Array
          is Access_All_Unconstrained_Array(11..20);

  subtype Access_All_11_20_Array_Aliased
          is Access_All_Unconstrained_Array_Aliased(11..20);


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

  -- these 'filler' functions create unique values for every element that
  -- is used and/or tested in this test.

  Well_Bottom : Integer := 0;

  function Filler( Size : Mod_128 ) return Unconstrained_Array is
    It : Unconstrained_Array( 0..Size-1 );
  begin
    for Eyes in It'Range loop
      It(Eyes) := Integer( Eyes ) + Well_Bottom;
    end loop;
    Well_Bottom := Well_Bottom + It'Length;
    return It;
  end Filler;

  function Filler( Size : Mod_128 ) return Unconstrained_Array_Aliased is
    It : Unconstrained_Array_Aliased( 0..Size-1 );
  begin
    for Ayes in It'Range loop
      It(Ayes) := Integer( Ayes ) + Well_Bottom;
    end loop;
    Well_Bottom := Well_Bottom + It'Length;
    return It;
  end Filler;

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

  An_Integer : Integer;

  type AAI is access all Integer;

  An_Integer_Access : AAI;

  Array_Item_01_10 : Array_01_10 := Filler(10); -- 0..9

  Array_Item_11_20 : Array_11_20 := Filler(10); -- 10..19 (sliding)

  Array_Aliased_Item_01_10 : Array_Aliased_01_10 := Filler(10); -- 20..29

  Array_Aliased_Item_11_20 : Array_Aliased_11_20 := Filler(10); -- 30..39

  Aliased_Array_Item_01_10 : aliased Array_01_10 := Filler(10); -- 40..49

  Aliased_Array_Item_11_20 : aliased Array_11_20 := Filler(10); -- 50..59

  Aliased_Array_Aliased_Item_01_10 : aliased Array_Aliased_01_10
                                   := Filler(10);               -- 60..69

  Aliased_Array_Aliased_Item_11_20 : aliased Array_Aliased_11_20
                                   := Filler(10);               -- 70..79

  Check_Item            : Access_All_Unconstrained_Array;

  Check_Aliased_Item    : Access_All_Unconstrained_Array_Aliased;

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

  procedure Fail( Message : String; CI, SB : Integer ) is
  begin
    Report.Failed("Wrong value passed " & Message);
    if Verbose then
      Report.Comment("got" & Integer'Image(CI) &
                     " should be" & Integer'Image(SB) );
    end if;
  end Fail;

  procedure Check_Array_01_10( Checked_Item : Array_01_10;
                               Low_SB       : Integer ) is
  begin
    for Index in Checked_Item'Range loop
      if (Checked_Item(Index) /= (Low_SB +Integer(Index)-1)) then
        Fail("unaliased 1..10", Checked_Item(Index),
                                (Low_SB +Integer(Index)-1));
      end if;
    end loop;
  end Check_Array_01_10;

  procedure Check_Array_11_20( Checked_Item : Array_11_20;
                               Low_SB       : Integer ) is
  begin
    for Index in Checked_Item'Range loop
      if (Checked_Item(Index) /= (Low_SB +Integer(Index)-11)) then
        Fail("unaliased 11..20", Checked_Item(Index),
                                 (Low_SB +Integer(Index)-11));
      end if;
    end loop;
 end Check_Array_11_20;

  procedure Check_Single_Integer( The_Integer, SB : Integer;
                                  Message         : String ) is
  begin
    if The_Integer /= SB then
      Report.Failed("Wrong integer value for " & Message );
    end if;
  end Check_Single_Integer;

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

begin  -- Main test procedure.

  Report.Test ("C360002", "Check that modular types may be used as array " &
                          "indices.  Check that if aliased appears in " &
                          "the component_definition of an array_type that " &
                          "each component of the array is aliased.  Check " &
                          "that references to aliased array objects " &
                          "produce correct results, and that out of bound " &
                          "references to aliased objects correctly " &
                          "produce Constraint_Error" );
  -- start with checks that the Filler assignments produced the expected
  -- result.  This is a "case 0" test to check that nothing REALLY surprising
  -- is happening

  Check_Array_01_10( Array_Item_01_10, 0 );
  Check_Array_11_20( Array_Item_11_20, 10 );

  -- check that having the variable aliased makes no difference
  Check_Array_01_10( Aliased_Array_Item_01_10, 40 );
  Check_Array_11_20( Aliased_Array_Item_11_20, 50 );

  -- now check that conversion between array types where the only
  -- difference in the definitions is that the components are aliased works

  Check_Array_01_10( Unconstrained_Array( Array_Aliased_Item_01_10 ), 20 );
  Check_Array_11_20( Unconstrained_Array( Array_Aliased_Item_11_20 ), 30 );

  -- check that conversion of an aliased object with aliased components
  -- also works

  Check_Array_01_10( Unconstrained_Array( Aliased_Array_Aliased_Item_01_10 ),
                     60 );
  Check_Array_11_20( Unconstrained_Array( Aliased_Array_Aliased_Item_11_20 ),
                     70 );

  -- check that the bounds will slide

  Check_Array_01_10( Array_01_10( Array_Item_11_20 ), 10 );
  Check_Array_11_20( Array_11_20( Array_Item_01_10 ),  0 );

  -- point at some of the components and check them

  An_Integer_Access := Array_Aliased_Item_01_10(5)'Access;

  Check_Single_Integer( An_Integer_Access.all, 24,
                       "Aliased component 'Access");

  An_Integer_Access := Aliased_Array_Aliased_Item_01_10(7)'Access;

  Check_Single_Integer( An_Integer_Access.all, 66,
                       "Aliased Aliased component 'Access");

  -- check some assignments

  Array_Item_01_10 := Aliased_Array_Item_01_10;
  Check_Array_01_10( Array_Item_01_10, 40 );

  Aliased_Array_Item_01_10 := Aliased_Array_Item_11_20(11..20);
  Check_Array_01_10( Aliased_Array_Item_01_10, 50 );

  Aliased_Array_Aliased_Item_11_20(11..20)
                                       := Aliased_Array_Aliased_Item_01_10;
  Check_Array_11_20( Unconstrained_Array( Aliased_Array_Aliased_Item_11_20 ),
                     60 );

  Report.Result;

end C360002;
