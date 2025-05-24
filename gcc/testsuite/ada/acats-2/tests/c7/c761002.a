-- C761002.A
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
--      Check that objects of a controlled type that are created
--      by an allocator are finalized at the appropriate time.  In 
--      particular, check that such objects are not finalized due to 
--      completion of the master in which they were allocated if the 
--      corresponding access type is declared outside of that master.
--
--      Check that Unchecked_Deallocation of a controlled 
--      object causes finalization of that object.
--
-- TEST DESCRIPTION:
--      This test derives a type from Ada.Finalization.Controlled, and
--      declares access types to that type in various scope scenarios.
--      The dispatching procedure Finalize is redefined for the derived
--      type to perform a check that it has been called at the 
--      correct time.  This is accomplished using a global variable 
--      which indicates what state the software is currently 
--      executing.  The test utilizes the TCTouch facilities to 
--      verify that Finalize is called the correct number of times, at 
--      the correct times.  Several calls are made to validate passing 
--      the null string to check that Finalize has NOT been called at 
--      that point.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

with Ada.Finalization;
package C761002_0 is
  type Global is new Ada.Finalization.Controlled with null record;
  procedure Finalize( It: in out Global );
  
  type Second is new Ada.Finalization.Limited_Controlled with null record;
  procedure Finalize( It: in out Second );
end C761002_0;

with Report;
with TCTouch;
package body C761002_0 is

  procedure Finalize( It: in out Global ) is
  begin
    TCTouch.Touch('F');  ------------------------------------------------- F
  end Finalize;

  procedure Finalize( It: in out Second ) is
  begin
    TCTouch.Touch('S');  ------------------------------------------------- S
  end Finalize;
end C761002_0;

with Report;
with TCTouch;
with C761002_0;
with Unchecked_Deallocation;
procedure C761002 is

  -- check the straightforward case
  procedure Subtest_1 is
    type Access_1 is access C761002_0.Global;
    V1 : Access_1;
    procedure Allocate is
      V2 : Access_1;
    begin
      V2 := new C761002_0.Global;
      V1 := V2;  -- "dead" assignment must not be optimized away due to
                 -- finalization "side effects", many more of these follow
    end Allocate;
  begin
    Allocate;
    -- no calls to Finalize should have occurred at this point
    TCTouch.Validate("","Allocated nested, retained");
  end Subtest_1;
  
  -- check Unchecked_Deallocation
  procedure Subtest_2 is
    type Access_2 is access C761002_0.Global;
    procedure Free is
              new Unchecked_Deallocation(C761002_0.Global, Access_2);
    V1 : Access_2;
    V2 : Access_2;

    procedure Allocate is
    begin
      V1 := new C761002_0.Global;
      V2 := new C761002_0.Global;
    end Allocate;

  begin
    Allocate;
    -- no calls to Finalize should have occurred at this point.
    TCTouch.Validate("","Allocated nested, non-local");
    
    Free(V1); -- instance of Unchecked_Deallocation
    -- should cause the finalization of V1.all
    TCTouch.Validate("F","Unchecked Deallocation");
  end Subtest_2; -- leaving this scope should cause the finalization of V2.all
  
  -- check various master-exit scenarios
  -- the "Fake" parameters are used to avoid unwanted optimizations
  procedure Subtest_3 is
    procedure With_Local_Block is
      type Access_3 is access C761002_0.Global;
      V1 : Access_3;
    begin
      declare
        V2 : Access_3 := new C761002_0.Global;
      begin
        V1 := V2;
      end;
      TCTouch.Validate("","Local Block, normal exit");
      -- the allocated object should be finalized on leaving this scope
    end With_Local_Block;
    
    procedure With_Local_Block_Return(Fake: Integer) is
      type Access_4 is access C761002_0.Global;
      V1 : Access_4 := new C761002_0.Global;
    begin
      if Fake = 0 then
        declare
          V2 : Access_4;
        begin
          V2 := new C761002_0.Global;
          return; -- the two allocated objects should be finalized
        end;      -- upon leaving this scope
      else
        V1 := null;
      end if;
    end With_Local_Block_Return;

    procedure With_Goto(Fake: Integer) is
      type Access_5 is access C761002_0.Global;
      V1 : Access_5 := new C761002_0.Global;
      V2 : Access_5;
      V3 : Access_5;
    begin
      if Fake = 0 then
        declare
          type Access_6 is access C761002_0.Second;
          V6 : Access_6;
        begin
          V6 := new C761002_0.Second;
          goto check;
        end;
      else
        V2 := V1;
      end if;
      V3 := V2;
<<check>>
      TCTouch.Validate("S","goto past master end");
     end With_Goto;

  begin
    With_Local_Block;
    TCTouch.Validate("F","Local Block, normal exit, after master");

    With_Local_Block_Return( Report.Ident_Int(0) );
    TCTouch.Validate("FF","Local Block, return from block");

    With_Goto( Report.Ident_Int(0) );
    TCTouch.Validate("F","With Goto");

  end Subtest_3;

  procedure Subtest_4 is

    Oops : exception;
    
    procedure Alley( Fake: Integer ) is
      type Access_1 is access C761002_0.Global;
      V1 : Access_1;
    begin
      V1 := new C761002_0.Global;
      if Fake = 1 then
        raise Oops;
      end if;
      V1 := null;
    end Alley;
  
  begin
    Catch: begin
      Alley( Report.Ident_Int(1) );
    exception
      when Oops   => TCTouch.Validate("F","leaving via exception");
      when others => Report.Failed("Wrong exception");
    end Catch;
  end Subtest_4;

begin  -- Main test procedure.

  Report.Test ("C761002", "Check that objects of a controlled type created "
                        & "by an allocator are finalized appropriately. " 
                        & "Check that Unchecked_Deallocation of a "
                        & "controlled object causes finalization " 
                        & "of that object" );
  
  Subtest_1;
  -- leaving the scope of the access type should finalize the 
  -- collection
  TCTouch.Validate("F","Allocated nested, Subtest 1");
  
  Subtest_2;
  -- Unchecked_Deallocation already finalized one of the two 
  -- objects allocated, the other should be the only one finalized 
  -- at leaving the scope of the access type.
  TCTouch.Validate("F","Allocated non-local");

  Subtest_3;
  -- there should be no remaining finalizations from this subtest
  TCTouch.Validate("","Localized objects");

  Subtest_4;
  -- there should be no remaining finalizations from this subtest
  TCTouch.Validate("","Exception testing");

  Report.Result;

end C761002;
