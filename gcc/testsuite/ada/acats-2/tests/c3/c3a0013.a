-- C3A0013.A
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
--      Check that a general access type object may reference allocated
--      pool objects as well as aliased objects. (3,4)
--      Check that formal parameters of tagged types are implicitly
--      defined as aliased; check that the 'Access of these formal
--      parameters designates the correct object with the correct
--      tag. (5)
--      Check that the current instance of a limited type is defined as
--      aliased. (5)
--
-- TEST DESCRIPTION:
--      This test takes from the hierarchy defined in C390003; making
--      the root type Vehicle limited private.  It also shifts the
--      abstraction to include the notion of a transmission, an object
--      which is contained within any vehicle.  Using an access
--      discriminant, any subprogram which operates on a transmission
--      may also reference the vehicle in which it is installed.
--
--      Class Hierarchy:
--              Vehicle         Transmission
--               /   \
--           Truck    Car
--
--      Contains:
--                Vehicle( Transmission )
--
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      16 Dec 94   SAIC    Fixed accessibility problems
--
--!

package C3A0013_1 is
  type Vehicle is tagged limited private;
  type Vehicle_ID is access all Vehicle'Class;

  -- Constructors
  procedure Create     ( It : in out Vehicle; 
                         Wheels : Natural := 4 );
  -- Modifiers
  procedure Accelerate ( It : in out Vehicle );
  procedure Decelerate ( It : in out Vehicle );
  procedure Up_Shift   ( It : in out Vehicle );
  procedure Stop       ( It : in out Vehicle );

  -- Selectors
  function  Speed      ( It : Vehicle ) return Natural;
  function  Wheels     ( It : Vehicle ) return Natural;
  function  Gear_Factor( It : Vehicle ) return Natural;

  -- TC_Ops
  procedure TC_Validate( It : in out Vehicle; Speed_Trap : Natural );

  -- dispatching procedure used to check tag correctness
  procedure TC_Validate( It     : Vehicle;
                         TC_ID  : Character);

private

  type Transmission(Within: access Vehicle'Class) is limited record
    Engaged : Boolean := False;
    Gear    : Integer range -1..5 := 0;
  end record;

  -- Current instance of a limited type is defined as aliased

  type Vehicle is tagged limited record
    Wheels: Natural;
    Speed : Natural;
    Power_Train: Transmission( Vehicle'Access );
  end record;
end C3A0013_1;

with C3A0013_1;
package C3A0013_2 is
  type Car is new C3A0013_1.Vehicle with private;
  procedure TC_Validate( It     : Car;
                         TC_ID  : Character);
  function  Gear_Factor( It : Car ) return Natural;
private
  type Car is new C3A0013_1.Vehicle with record
    Displacement : Natural;
  end record;
end C3A0013_2;

with C3A0013_1;
package C3A0013_3 is
  type Truck is new C3A0013_1.Vehicle with private;
  procedure TC_Validate( It     : Truck;
                         TC_ID  : Character);
  function  Gear_Factor( It : Truck ) return Natural;
private
  type Truck is new C3A0013_1.Vehicle with record
    Displacement : Natural;
  end record;
end C3A0013_3;

with Report;
package body C3A0013_1 is

  procedure Create    ( It : in out Vehicle; 
                        Wheels : Natural := 4 ) is
  begin
    It.Wheels   := Wheels;
    It.Speed    := 0;
  end Create;

  procedure Accelerate( It : in out Vehicle ) is
  begin
    It.Speed := It.Speed + Gear_Factor( It.Power_Train.Within.all );
  end Accelerate;

  procedure Decelerate( It : in out Vehicle ) is
  begin
    It.Speed := It.Speed - Gear_Factor( It.Power_Train.Within.all );
  end Decelerate;

  procedure Stop      ( It : in out Vehicle ) is
  begin
    It.Speed := 0;
    It.Power_Train.Engaged := False;
  end Stop;

  function  Gear_Factor( It : Vehicle ) return Natural is
  begin
    return It.Power_Train.Gear;
  end Gear_Factor;

  function  Speed     ( It : Vehicle ) return Natural is
  begin
    return It.Speed;
  end Speed;

  function  Wheels     ( It : Vehicle ) return Natural is
  begin
    return It.Wheels;
  end Wheels;

  -- formal tagged parameters are implicitly aliased

  procedure TC_Validate( It : in out Vehicle; Speed_Trap : Natural ) is
    License: Vehicle_ID := It'Unchecked_Access;
  begin
    if Speed( License.all ) /= Speed_Trap then
      Report.Failed("Speed Trap: expected: " & Natural'Image(Speed_Trap));
    end if;
  end TC_Validate;

  procedure TC_Validate( It     : Vehicle;
                         TC_ID  : Character) is
  begin
    if TC_ID /= 'V' then
      Report.Failed("Dispatched to Vehicle");
    end if;
    if Wheels( It ) /= 1 then
      Report.Failed("Not a Vehicle");
    end if;
  end TC_Validate;

  procedure Up_Shift( It: in out Vehicle ) is
  begin
    It.Power_Train.Gear    := It.Power_Train.Gear +1;
    It.Power_Train.Engaged := True;
    Accelerate( It );
  end Up_Shift;
end C3A0013_1;

with Report;
package body C3A0013_2 is

  procedure TC_Validate( It     : Car;
                         TC_ID  : Character ) is
  begin
    if TC_ID /= 'C' then
      Report.Failed("Dispatched to Car");
    end if;
    if Wheels( It ) /= 4 then
      Report.Failed("Not a Car");
    end if;
  end TC_Validate;

  function  Gear_Factor( It : Car ) return Natural is
  begin
    return C3A0013_1.Gear_Factor( C3A0013_1.Vehicle( It ) )*2;
  end Gear_Factor;

end C3A0013_2;

with Report;
package body C3A0013_3 is

  procedure TC_Validate( It     : Truck;
                         TC_ID  : Character) is
  begin
    if TC_ID /= 'T' then
      Report.Failed("Dispatched to Truck");
    end if;
    if Wheels( It ) /= 3 then
      Report.Failed("Not a Truck");
    end if;
  end TC_Validate;

  function  Gear_Factor( It : Truck ) return Natural is
  begin
    return C3A0013_1.Gear_Factor( C3A0013_1.Vehicle( It ) )*3;
  end Gear_Factor;

end C3A0013_3;

package C3A0013_4 is
  procedure Perform_Tests;
end C3A0013_4;

with Report;
with C3A0013_1;
with C3A0013_2;
with C3A0013_3;
package body C3A0013_4 is
  package Root   renames C3A0013_1;
  package Cars   renames C3A0013_2;
  package Trucks renames C3A0013_3;

  type Car_Pool is array(1..4) of aliased Cars.Car;
  Commuters : Car_Pool;

  My_Car      : aliased Cars.Car;
  Company_Car : Root.Vehicle_ID;
  Repair_Shop : Root.Vehicle_ID;

  The_Vehicle : Root.Vehicle;
  The_Car     : Cars.Car;
  The_Truck   : Trucks.Truck;

  procedure TC_Dispatch( Ptr   : Root.Vehicle_ID;
                         Char  : Character ) is
  begin
    Root.TC_Validate( Ptr.all, Char );
  end TC_Dispatch;

  procedure TC_Check_Formal_Access( Item: in out Root.Vehicle'Class;
                                    Char: Character) is
  begin
    TC_Dispatch( Item'Unchecked_Access, Char );
  end TC_Check_Formal_Access;

  procedure Perform_Tests is
  begin  -- Main test procedure.

  for Lane in Commuters'Range loop
    Cars.Create( Commuters(Lane) );
    for Excitement in 1..Lane loop
      Cars.Up_Shift( Commuters(Lane) );
    end loop;
  end loop;

  Cars.Create( My_Car );
  Cars.Up_Shift( My_Car );
  Cars.TC_Validate( My_Car, 2 );

  Root.Create( The_Vehicle, 1 );
  Cars.Create( The_Car    , 4 );
  Trucks.Create( The_Truck, 3 );

  TC_Check_Formal_Access( The_Vehicle, 'V' );
  TC_Check_Formal_Access( The_Car,     'C' );
  TC_Check_Formal_Access( The_Truck,   'T' );

  Root.Up_Shift( The_Vehicle );
  Cars.Up_Shift( The_Car );
  Trucks.Up_Shift( The_Truck );

  Root.TC_Validate( The_Vehicle, 1 ); 
  Cars.TC_Validate( The_Car, 2 ); 
  Trucks.TC_Validate( The_Truck, 3 ); 

  --  general access type may reference allocated objects

  Company_Car := new Cars.Car;
  Root.Create( Company_Car.all );
  Root.Up_Shift( Company_Car.all );
  Root.Up_Shift( Company_Car.all );
  Root.TC_Validate( Company_Car.all, 6 );

  --  general access type may reference aliased objects

  Repair_Shop := My_Car'Access;
  Root.TC_Validate( Repair_Shop.all, 2 );

  --  general access type may reference aliased objects

  Construction: declare
    type Speed_List is array(Commuters'Range) of Natural;
    Accelerations : constant Speed_List := (2, 6, 12, 20);
  begin
    for Rotation in Commuters'Range loop
      Repair_Shop := Commuters(Rotation)'Access;
      Root.TC_Validate( Repair_Shop.all, Accelerations(Rotation) );
    end loop;
  end Construction;

end Perform_Tests;

end C3A0013_4;

with C3A0013_4;
with Report;
procedure C3A0013 is
begin

  Report.Test ("C3A0013", "Check general access types.  Check aliased "
                        & "nature of formal tagged type parameters.  "
                        & "Check aliased nature of the current "
                        & "instance of a limited type.  Check the "
                        & "constraining of actual subtypes for "
                        & "discriminated objects" );

  C3A0013_4.Perform_Tests;

  Report.Result;
end C3A0013;
