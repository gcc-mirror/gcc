-- C392C07.A
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
--     the case where the call has dynamic tagged controlling operands
--     of the type T.  Check for calls to these same subprograms where
--     the operands are of specific statically tagged types:
--     objects (declared or allocated), formal parameters, view
--     conversions, and function calls (both primitive and non-primitive).
--
-- TEST DESCRIPTION:
--      This test uses foundation F392C00 to test the usages of statically
--      tagged objects and values.  This test is derived in part from
--      C392C05.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      24 Oct 95   SAIC    Updated for ACVC 2.0.1
--
--!

with Report;
with TCTouch;
with F392C00_1;
procedure C392C07 is -- Hardware_Store
  package Switch renames F392C00_1;

  subtype Switch_Class is Switch.Toggle'Class;

  type Reference is access all Switch_Class;

  A_Switch   : aliased Switch.Toggle;
  A_Dimmer   : aliased Switch.Dimmer;
  An_Autodim : aliased Switch.Auto_Dimmer;

  type Light_Bank is array(Positive range <>) of Reference;

  Lamps : Light_Bank(1..3);

-- dynamically tagged controlling operands : class wide formal parameters
  procedure Clamp( Device : in out Switch_Class; On : Boolean := False ) is
  begin
    if Switch.On( Device ) /= On then                  
      Switch.Flip( Device );                           
    end if;
  end Clamp;
  function Class_Item(Bank_Pos: Positive) return Switch_Class is
  begin
    return Lamps(Bank_Pos).all;
  end Class_Item;

begin  -- Main test procedure.
  Report.Test ("C392C07", "Check that a dispatching subprogram call is "
                        & "determined by the controlling tag for "
                        & "dynamically tagged controlling operands" );

  Lamps := ( A_Switch'Access, A_Dimmer'Access, An_Autodim'Access );

-- dynamically tagged operands referring to
-- statically tagged declared objects
  for Knob in Lamps'Range loop
    Clamp( Lamps(Knob).all, On => True );
  end loop;
  TCTouch.Validate( "BABGBABKGBA", "Clamping On Lamps" );

  Lamps(1) := new Switch.Toggle;
  Lamps(2) := new Switch.Dimmer;
  Lamps(3) := new Switch.Auto_Dimmer;

-- turn the full bank of switches ON
-- dynamically tagged allocated objects
  for Knob in Lamps'Range loop
    Clamp( Lamps(Knob).all, On => True );
  end loop;
  TCTouch.Validate( "BABGBABKGBA", "Dynamic Allocated");

-- Double check execution correctness
  if Switch.Off( Lamps(1).all )
     or Switch.Off( Lamps(2).all )
     or Switch.Off( Lamps(3).all ) then
    Report.Failed( "Bad Value" );
  end if;
  TCTouch.Validate( "CCC", "Class-wide");

-- turn the full bank of switches OFF
  for Knob in Lamps'Range loop
    Switch.Flip( Lamps(Knob).all );
  end loop;
  TCTouch.Validate( "AGBAKGBA", "Dynamic Allocated, Primitive Ops");

-- check switches for OFF
-- a few function calls as operands
  for Knob in Lamps'Range loop
    if not Switch.Off( Class_Item(Knob) ) then
      Report.Failed("At function tests, Switch not OFF");
    end if;
  end loop;
  TCTouch.Validate( "CCC",
                         "Using function returning class-wide type");

-- Switches are all OFF now.
-- dynamically tagged view conversion
  Clamp( Switch_Class( A_Switch ) );         
  Clamp( Switch_Class( A_Dimmer ) );         
  Clamp( Switch_Class( An_Autodim ) );       
  TCTouch.Validate( "BABGBABKGBA", "View Conversions" );

-- dynamically tagged controlling operands : declared class wide objects
--  calling primitive functions
  declare
    Dine_O_Might : Switch_Class := Switch.TC_CW_TI( 't' );  
  begin
    Switch.Flip( Dine_O_Might );                            
    if Switch.On( Dine_O_Might ) then                       
      Report.Failed( "Exploded at Dine_O_Might" );
    end if;
    TCTouch.Validate( "WAB", "Dispatching function 1" );
  end;

  declare
    Dyne_A_Mite : Switch_Class := Switch.TC_CW_TI( 'd' );   
  begin
    Switch.Flip( Dyne_A_Mite );                             
    if Switch.On( Dyne_A_Mite ) then                        
      Report.Failed( "Exploded at Dyne_A_Mite" );
    end if;
    TCTouch.Validate( "WGBAB", "Dispatching function 2" );
  end;

  declare
    Din_Um_Out : Switch_Class := Switch.TC_CW_TI( 'a' );    
  begin
    Switch.Flip( Din_Um_Out );                              
    if Switch.Off( Din_Um_Out ) then                        
      Report.Failed( "Exploded at Din_Um_Out" );
    end if;
    TCTouch.Validate( "WKCC", "Dispatching function 3" );

-- Non-dispatching function calls.
    if not Switch.TC_Non_Disp( Switch.Toggle( Din_Um_Out ) ) then
      Report.Failed( "Non primitive, via view conversion" );
    end if;
    TCTouch.Validate( "X", "View Conversion 1" );

    if not Switch.TC_Non_Disp( Switch.Dimmer( Din_Um_Out ) ) then
      Report.Failed( "Non primitive, via view conversion" );
    end if;
    TCTouch.Validate( "Y", "View Conversion 2" );
  end;

  -- a few more function calls as operands (oops)
  if not Switch.On( Switch.Toggle'( Switch.Create ) ) then 
    Report.Failed("Toggle did not create ""On""");
  end if;

  if Switch.Off( Switch.Dimmer'( Switch.Create ) ) then 
    Report.Failed("Dimmer created ""Off""");
  end if;

  if Switch.Off( Switch.Auto_Dimmer'( Switch.Create ) ) then 
    Report.Failed("Auto_Dimmer created ""Off""");
  end if;

  Report.Result;
end C392C07;
