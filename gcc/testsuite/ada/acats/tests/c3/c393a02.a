-- C393A02.A
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
--     Check that a dispatching call to an abstract subprogram invokes
--     the correct subprogram body of a descendant type according to
--     the controlling tag.
--     Check that a subprogram can be declared with formal parameters
--     and result that are of an abstract type's associated class-wide
--     type and that such subprograms can be called. 3.4.1(4)
--
-- TEST DESCRIPTION:
--     This test declares several objects of types derived from the
--     abstract type as defined in the foundation F393A00.  It then calls
--     various dispatching and class-wide subprograms using those objects.
--     The packages in F393A00 are instrumented to trace the flow of
--     execution.
--     The test checks for the correct order of execution, as expected
--     by the various calls.
--
-- TEST FILES:
--     The following files comprise this test:
--
--        F393A00.A   (foundation code)
--        C393A02.A
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      19 Dec 94   SAIC    Removed RM references from objective text.
--      05 APR 96   SAIC    Update RM references for 2.1
--
--!

with Report;
with F393A00_0;
with F393A00_1;
with F393A00_2;
with F393A00_3;
with F393A00_4;
procedure C393A02 is

  A_Windmill : F393A00_2.Windmill;
  A_Pump     : F393A00_3.Pump;
  A_Mill     : F393A00_4.Mill;

  A_Windmill_2 : F393A00_2.Windmill;
  A_Pump_2     : F393A00_3.Pump;
  A_Mill_2     : F393A00_4.Mill;

  B_Windmill : F393A00_2.Windmill;
  B_Pump     : F393A00_3.Pump;
  B_Mill     : F393A00_4.Mill;

  procedure Swapem( A,B: in out F393A00_2.Windmill'Class ) is
  begin
   F393A00_0.TC_Touch('x');
   F393A00_2.Swap( A,B );
  end Swapem;

  function Zephyr( A: F393A00_2.Windmill'Class )
           return F393A00_2.Windmill'Class is
    Item : F393A00_2.Windmill'Class := A;
  begin
    F393A00_0.TC_Touch('y');
    if not F393A00_1.Initialized( Item ) then  -- b
      F393A00_2.Initialize( Item );            -- a
    end if;
    F393A00_2.Stop( Item );                    -- f / mff
    F393A00_2.Add_Spin( Item, 10 );            -- e
    return Item;
  end Zephyr;

  function Gale( It: F393A00_2.Windmill ) return F393A00_2.Windmill'Class is
    Item : F393A00_2.Windmill'Class := It;
  begin
    F393A00_2.Stop( Item );                   -- f
    F393A00_2.Add_Spin( Item, 40 );           -- e
    return Item;
  end Gale;

  function Gale( It: F393A00_3.Pump ) return F393A00_2.Windmill'Class is
    Item : F393A00_2.Windmill'Class := It;
  begin 
    F393A00_2.Stop( Item );                   -- f
    F393A00_2.Add_Spin( Item, 50 );           -- e
    return Item;
  end Gale;

  function Gale( It: F393A00_4.Mill ) return F393A00_2.Windmill'Class is
    Item : F393A00_2.Windmill'Class := It;
  begin 
    F393A00_2.Stop( Item );                   -- mff
    F393A00_2.Add_Spin( Item, 60 );           -- e
    return Item;
  end Gale;

begin  -- Main test procedure.

  Report.Test ("C393A02", "Check that a dispatching call to an abstract "
                         & "subprogram invokes the correct subprogram body. "
                         & "Check that a subprogram declared with formal "
                         & "parameters/result of an abstract type's "
                         & "associated class-wide can be called" );

  F393A00_0.TC_Validate( "hhh", "Mill declarations" );
  A_Windmill := F393A00_2.Create;
  F393A00_0.TC_Validate( "d", "Create A_Windmill" );

  A_Pump     := F393A00_3.Create;
  F393A00_0.TC_Validate( "h", "Create A_Pump" );

  A_Mill     := F393A00_4.Create;
  F393A00_0.TC_Validate( "hl", "Create A_Mill" );

  --------------

  Swapem( A_Windmill, A_Windmill_2 );
  F393A00_0.TC_Validate( "xc", "Windmill Swap" );
  
  Swapem( A_Pump, A_Pump_2 );
  F393A00_0.TC_Validate( "xc", "Pump Swap" );
  
  Swapem( A_Mill, A_Mill_2 );
  F393A00_0.TC_Validate( "xk", "Pump Swap" );

  F393A00_2.Initialize( A_Windmill_2 );  
  F393A00_3.Initialize( A_Pump_2 );      
  F393A00_4.Initialize( A_Mill_2 );      
  B_Windmill := A_Windmill_2;
  B_Pump     := A_Pump_2;
  B_Mill     := A_Mill_2;                
  F393A00_2.Add_Spin( B_Windmill, 123 ); 
  F393A00_3.Set_Rate( B_Pump, 12.34 );   
  F393A00_4.Add_Spin( B_Mill, 321 );     
  F393A00_0.TC_Validate( "aaaeie", "Setting Values" );

  declare
    It : F393A00_2.Windmill'Class := Zephyr( B_Windmill ); -- ybfe
    XX : F393A00_2.Windmill'Class := Gale( B_Windmill );   -- fe
    use type F393A00_2.Rotational_Measurement;
  begin
    if not F393A00_1.Initialized( It ) or not F393A00_1.Initialized( XX )
then
      Report.Failed( "Copy to class-wide variable" );
    end if;                                                -- bb
    if F393A00_2.Spin( It ) /= 10                          -- g
       or F393A00_2.Spin( XX ) /= 40 then                  -- g
      Report.Failed( "Call to class-wide operation" );
    end if;

    F393A00_0.TC_Validate( "ybfefebbgg", "Windmill Zephyr" );
  end;

  declare
    It : F393A00_2.Windmill'Class := Zephyr( B_Pump );     -- ybfe
    XX : F393A00_2.Windmill'Class := Gale( B_Pump );       -- fe
    use type F393A00_2.Rotational_Measurement;
  begin
    if not F393A00_1.Initialized( It ) or not F393A00_1.Initialized( XX )
then
      Report.Failed( "Bad copy to class-wide variable" );
    end if;                                                -- bb
    if F393A00_2.Spin( It ) /= 10                          -- g
       or F393A00_2.Spin( XX ) /= 50 then                  -- g
      Report.Failed( "Call to class-wide operation" );
    end if;

    F393A00_0.TC_Validate( "ybfefebbgg", "Pump Zephyr" );
  end;

  declare
    It : F393A00_2.Windmill'Class := Zephyr( B_Mill );     -- ybmffe
    XX : F393A00_2.Windmill'Class := Gale( B_Mill );       -- mffe
    use type F393A00_2.Rotational_Measurement;
  begin
    if not F393A00_1.Initialized( It ) or not F393A00_1.Initialized( XX )
then
      Report.Failed( "Bad copy to class-wide variable" );
    end if;                                                -- bb
    if F393A00_2.Spin( It ) /= 10                          -- g
       or F393A00_2.Spin( XX ) /= 60 then                  -- g
      Report.Failed( "Call to class-wide operation" );
    end if;

    F393A00_0.TC_Validate( "ybmffemffebbgg", "Mill Zephyr" );
  end;

  Report.Result;

end C393A02;
