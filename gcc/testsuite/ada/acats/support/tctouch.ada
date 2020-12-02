-- TCTouch.A
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
-- FOUNDATION DESCRIPTION:
--      The tools in this foundation are not peculiar to any particular
--      aspect of the language, but simplify the test writing and reading
--      process.  Assert and Assert_Not are used to reduce the textual
--      overhead of the test-that-this-condition-is-(not)-true paradigm.
--      Touch and Validate are used to simplify tracing an expected path
--      of execution.
--      A tag comment of the form:
--
--      TCTouch.Touch( 'A' ); ----------------------------------------- A
--
--      is recommended to improve readability of this feature.
--
--      Report.Test must be called before any of the procedures in this
--      package with the exception of Touch.
--      The usage paradigm is to call Touch in locations in the test where you
--      want a trace of execution.  Each call to Touch should have a unique
--      character associated with it.  At each place where a check can
--      reasonably be performed to determine correct execution of a
--      sub-test, a call to Validate should be made.  The first parameter
--      passed to Validate is the expected string of characters produced by
--      call(s) to Touch in the subtest just executed.  The second parameter
--      is the message to pass to Report.Failed if the expected sequence was
--      not executed.
--
--      Validate should always be called after calls to Touch before a test
--      completes.
--
--      In the event that calls may have been made to Touch that are not
--      intended to be recorded, or, the failure of a previous subtest may
--      leave Touch calls "Unvalidated", the procedure Flush will reset the
--      tracker to the "empty" state.  Flush does not make any calls to
--      Report.
--
--      Calls to Assert and Assert_Not are to replace the idiom:
--
--         if BadCondition then  -- or if not PositiveTest then
--           Report.Failed(Message);
--         end if;
--
--      with:
--
--         Assert_Not( BadCondition, Message ); -- or
--         Assert( PositiveTest, Message );
--
--      Implementation_Check is for use with tests that cross the boundary
--      between the core and the Special Needs Annexes.  There are several
--      instances where language in the core becomes enforceable only when
--      a Special Needs Annex is supported.  Implementation_Check should be
--      called in place of Report.Failed in these cases; it examines the
--      constants in Impdef that indicate if the particular Special Needs
--      Annex is being validated with this validation; and acts accordingly.
--
--      The constant Foundation_ID contains the internal change version
--      for this software.
--
-- ERROR CONDITIONS:
--
--      It is an error to perform more than Max_Touch_Count (80) calls to
--      Touch without a subsequent call to Validate.  To do so will cause
--      a false test failure.
--
-- CHANGE HISTORY:
--     02 JUN 94   SAIC    Initial version
--     27 OCT 94   SAIC    Revised version
--     07 AUG 95   SAIC    Added Implementation_Check
--     07 FEB 96   SAIC    Changed to match new Impdef for 2.1
--     16 MAR 00   RLB     Changed foundation id to reflect test suite version.
--     22 MAR 01   RLB     Changed foundation id to reflect test suite version.
--     29 MAR 02   RLB     Changed foundation id to reflect test suite version.
--     06 MAR 07   RLB     Changed foundation id to reflect test suite version.
--
--!

package TCTouch is
  Foundation_ID   : constant String := "TCTouch ACATS 2.6";
  Max_Touch_Count : constant        := 80;

  procedure Assert    ( SB_True  : Boolean; Message : String );
  procedure Assert_Not( SB_False : Boolean; Message : String );

  procedure Touch   ( A_Tag   : Character );
  procedure Validate( Expected: String;
                      Message : String;
                      Order_Meaningful : Boolean := True );

  procedure Flush;

  type Special_Needs_Annexes is ( Annex_C, Annex_D, Annex_E,
                                  Annex_F, Annex_G, Annex_H );

  procedure Implementation_Check( Message : in String;
                                  Annex   : in Special_Needs_Annexes
                                          := Annex_C );
    -- If Impdef.Validating_Annex_<Annex> is true, will call Report.Failed
    -- otherwise will call Report.Not_Applicable.  This is to allow tests
    -- which are driven by wording in the core of the language, yet have
    -- their functionality dictated by the Special Needs Annexes to perform
    -- dual purpose.
    -- The default of Annex_C for the Annex parameter is to support early
    -- tests written with the assumption that Implementation_Check was
    -- expressly for use with the Systems Programming Annex.

end TCTouch;

with Report;
with Impdef;
package body TCTouch is

  procedure Assert( SB_True : Boolean; Message : String ) is
  begin
    if not SB_True then
      Report.Failed( "Assertion failed: " & Message );
    end if;
  end Assert;

  procedure Assert_Not( SB_False : Boolean; Message : String ) is
  begin
    if SB_False then
      Report.Failed( "Assertion failed: " & Message );
    end if;
  end Assert_Not;

  Collection : String(1..Max_Touch_Count);
  Finger     : Natural := 0;

  procedure Touch ( A_Tag : Character ) is
  begin
    Finger := Finger+1;
    Collection(Finger) := A_Tag;
  exception
    when Constraint_Error =>
      Report.Failed("Trace Overflow: " & Collection);
      Finger := 0;
  end Touch;

  procedure Sort_String( S: in out String ) is
  -- algorithm from Booch Components Page 472
    No_Swaps : Boolean;
    procedure Swap(C1, C2: in out Character) is
      T: Character := C1;
    begin  C1 := C2; C2 := T; end Swap;
  begin
    for OI in S'First+1..S'Last loop
      No_Swaps := True;
      for II in reverse OI..S'Last loop
        if S(II) < S(II-1) then
          Swap(S(II),S(II-1));
          No_Swaps := False;
        end if;
      end loop;
      exit when No_Swaps;
    end loop;
  end Sort_String;

  procedure Validate( Expected: String;
                      Message : String;
                      Order_Meaningful : Boolean := True) is
    Want : String(1..Expected'Length) := Expected;
  begin
    if not Order_Meaningful then
      Sort_String( Want );
      Sort_String( Collection(1..Finger) );
    end if;
    if Collection(1..Finger) /= Want then
      Report.Failed( Message & " Expecting: " & Want
			     & " Got: " & Collection(1..Finger) );
    end if;
    Finger := 0;
  end Validate;

  procedure Flush is
  begin
    Finger := 0;
  end Flush;

  procedure Implementation_Check( Message : in String;
                                  Annex   : in Special_Needs_Annexes
                                          := Annex_C ) is
                                          -- default to cover some legacy
  -- USAGE DISCIPLINE:
  --   Implementation_Check is designed to be used in tests that have
  --   interdependency on one of the Special Needs Annexes, yet are _really_
  --   tests based in the core language.  There will be instances where the
  --   execution of a test would be failing in the light of the requirements
  --   of the annex, yet from the point of view of the core language without
  --   the additional requirements of the annex, the test does not apply.
  --   In these cases, rather than issuing a call to Report.Failed, calling
  --   TCTouch.Implementation_Check will check that sensitivity, and if
  --   the implementation is attempting to validate against the specific
  --   annex, Report.Failed will be called, otherwise, Report.Not_Applicable
  --   will be called.
  begin

    case Annex is
      when Annex_C =>
        if ImpDef.Validating_Annex_C then
          Report.Failed( Message );
        else
          Report.Not_Applicable( Message & " Annex C not supported" );
       end if;

      when Annex_D =>
        if ImpDef.Validating_Annex_D then
          Report.Failed( Message );
        else
          Report.Not_Applicable( Message & " Annex D not supported" );
       end if;

      when Annex_E =>
        if ImpDef.Validating_Annex_E then
          Report.Failed( Message );
        else
          Report.Not_Applicable( Message & " Annex E not supported" );
       end if;

      when Annex_F =>
        if ImpDef.Validating_Annex_F then
          Report.Failed( Message );
        else
          Report.Not_Applicable( Message & " Annex F not supported" );
       end if;

      when Annex_G =>
        if ImpDef.Validating_Annex_G then
          Report.Failed( Message );
        else
          Report.Not_Applicable( Message & " Annex G not supported" );
       end if;

      when Annex_H =>
        if ImpDef.Validating_Annex_H then
          Report.Failed( Message );
        else
          Report.Not_Applicable( Message & " Annex H not supported" );
       end if;
    end case;
 end Implementation_Check;

end TCTouch;
