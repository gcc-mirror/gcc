-- C392C05.A
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
--     Check that for a call to a dispatching subprogram the subprogram
--     body which is executed is determined by the controlling tag for
--     the case where the call has statically tagged controlling operands
--     of the type T.  Check this for various operands of tagged types:
--     objects (declared or allocated), formal parameters, view conversions,
--     function calls (both primitive and non-primitive).
--
-- TEST DESCRIPTION:
--      This test uses foundation F392C00 to test the usages of statically
--      tagged objects and values.  The calls to Validate indicate the
--      expected sequence of procedure calls since the previous call to
--      Validate.  Static tags can be determined at compile time, and
--      hence this is a test of correct overload resolution for tagged types.
--      A clever compiler which unrolls loops and does path analysis on
--      access values will be able to perform the same kind of determination
--      for all of the code in this test.
--
-- TEST FILES:
--      The following files comprise this test:
--
--         F392C00.A   (foundation code)
--         C392C05.A
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      19 Dec 94   SAIC    Removed RM references from objective text.
--      24 Oct 95   SAIC    Updated for ACVC 2.0.1
--      13 Feb 97   PWB.CTA Corrected assumption that "or" operands are
--                          evaluated in textual order.
--!

with Report;
with TCTouch;
with F392C00_1;
procedure C392C05 is -- Hardware_Store

  package Switch renames F392C00_1;

  subtype Switch_Class is Switch.Toggle'Class;

  type Reference is access all Switch_Class;

  A_Switch   : aliased Switch.Toggle;
  A_Dimmer   : aliased Switch.Dimmer;
  An_Autodim : aliased Switch.Auto_Dimmer;

  type Light_Bank is array(Positive range <>) of Reference;

  Lamps : Light_Bank(1..3);

begin  -- Main test procedure.

  Report.Test ("C392C05", "Check that a dispatching subprogram call is "
                        & "determined by the controlling tag for statically "
                        & "tagged controlling operands" );

-- Check use of static tagged declared objects,
--   and static tagged formal parameters
-- Must call correct version of flip based on type of controlling op.

-- Turn on the lights!

  Switch.Flip( A_Switch );
  TCTouch.Validate( "A", "Declared Toggle" );

  Switch.Flip( A_Dimmer );
  TCTouch.Validate( "GBA", "Declared Dimmer" );

  Switch.Flip( An_Autodim );
  TCTouch.Validate( "KGBA", "Declared Auto_Dimmer" );

  Lamps(1) := new Switch.Toggle;
  Lamps(2) := new Switch.Dimmer;
  Lamps(3) := new Switch.Auto_Dimmer;

-- Check use of static tagged allocated objects,
--   and static tagged formal parameters in a loop which may dynamically
--   dispatch.  If an optimizer unrolls the loop, it may then be statically
--   determined, and no dispatching will occur.  Either interpretation is
--   correct.
  for Knob in Lamps'Range loop
    Switch.Flip( Lamps(Knob).all );
  end loop;
  TCTouch.Validate( "AGBAKGBA", "Allocated Objects" );

-- Check use of static tagged declared objects,
--   calling non-primitive functions.
  if not Switch.TC_Non_Disp( A_Switch ) then
    Report.Failed( "Bad Value 1" );
  end if;
  TCTouch.Validate( "X", "Nonprimitive Function" );

  if not Switch.TC_Non_Disp( A_Dimmer ) then
    Report.Failed( "Bad Value 2" );
  end if;
  TCTouch.Validate( "Y", "Nonprimitive Function" );

  if not Switch.TC_Non_Disp( An_Autodim ) then
    Report.Failed( "Bad Value 3" );
  end if;
  TCTouch.Validate( "Z", "Nonprimitive Function" );

  A_Switch   := Switch.Create;
  A_Dimmer   := Switch.Create;
  An_Autodim := Switch.Create;
  TCTouch.Validate( "123", "Primitive Function" );

-- View conversions
  Switch.Brighten( An_Autodim, 50 ); 

  Switch.Flip( Switch.Toggle( A_Switch ) );
  Switch.Flip( Switch.Toggle( A_Dimmer ) );
  Switch.Flip( Switch.Dimmer( An_Autodim ) );
  TCTouch.Validate( "DAAGBA", "View Conversions" );

-- statically tagged controlling operands (specific types) provided to
-- class-wide functions
  if Switch.On( A_Switch )
     or Switch.On( A_Dimmer )
     or Switch.On( An_Autodim ) then
    Report.Failed( "Bad Value 4" );
  end if;
  TCTouch.Validate( "BBB", "Class-wide" );

-- statically tagged controlling operands qualified expressions provided to
-- primitive functions, also using context to determine call to a
-- class-wide function.
  if Switch.Off( Switch.Toggle'( Switch.Create ) )
     or else Switch.Off( Switch.Dimmer'( Switch.Create ) )
     or else Switch.Off( Switch.Auto_Dimmer'( Switch.Create ) ) then
    Report.Failed( "Bad Value 5" );
  end if;
  TCTouch.Validate( "1C2C3C", "Qualified Expression/Class-Wide" );

  Report.Result;

end C392C05;
