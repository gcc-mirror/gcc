-- C390002.A
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
--      Check that a tagged base type may be declared, and derived
--      from in simple, private and extended forms.  (Overlaps with C390B04)
--      Check that the package Ada.Tags is present and correctly implemented.
--      Check for the correct operation of Expanded_Name, External_Tag and
--      Internal_Tag within that package.  Check that the exception Tag_Error
--      is correctly raised on calling Internal_Tag with bad input.
--
-- TEST DESCRIPTION:
--      This test declares a tagged type, and derives three types from it.
--      These types are then used to test the presence and function of the
--      package Ada.Tags.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      19 Dec 94   SAIC    Removed RM references from objective text.
--      27 Jan 96   SAIC    Update RM references for 2.1
--
--!

with Report;
with Ada.Tags;  

procedure C390002 is

  package Vehicle is

    type Object is tagged limited private;  -- ancestor type
    procedure Create( The_Vehicle : in out Object; Wheels : in Natural );
    function  Wheels( The_Vehicle : Object ) return Natural;

  private

    type Object is tagged limited record
      Wheel_Count : Natural := 0;
    end record;

  end Vehicle;

  package Motivators is

    type Bicycle is new Vehicle.Object with null record; -- simple 

    type Car is new Vehicle.Object with record           -- extended
      Convertible : Boolean;
    end record;

    type Truck is new Vehicle.Object with private;       -- private

  private

    type Truck is new Vehicle.Object with record
      Air_Horn : Boolean;
    end record;

  end Motivators;

  package body Vehicle is

    procedure Create( The_Vehicle : in out Object; Wheels : in Natural ) is
    begin
      The_Vehicle.Wheel_Count := Wheels;
    end Create;

    function  Wheels( The_Vehicle : Object ) return Natural is
    begin
      return The_Vehicle.Wheel_Count;
    end Wheels;

  end Vehicle;

  function TC_ID_Tag( Tag : in Ada.Tags.Tag ) return Ada.Tags.Tag is
  begin
    return Ada.Tags.Internal_Tag( Ada.Tags.External_Tag( Tag ) );
    Report.Comment("This message intentionally blank.");
  end TC_ID_Tag;

  procedure Check_Tags( Machine       : in Vehicle.Object'Class;
                        Expected_Name : in String;
                        External_Tag  : in String ) is
    The_Tag : constant Ada.Tags.Tag := Machine'Tag;
    use type Ada.Tags.Tag;
  begin
      if Ada.Tags.Expanded_Name(The_Tag) /= Expected_Name then  
         Report.Failed ("Failed in Check_Tags, Expanded_Name "
                        & Expected_Name);
      end if;
      if Ada.Tags.External_Tag(The_Tag) /= External_Tag then  
         Report.Failed ("Failed in Check_Tags, External_Tag "
                        & Expected_Name);
      end if;
      if Ada.Tags.Internal_Tag(External_Tag) /= The_Tag then
         Report.Failed ("Failed in Check_Tags, Internal_Tag "
                        & Expected_Name);
      end if;
  end Check_Tags;

  procedure Check_Exception is
    Boeing_777_Id : Ada.Tags.Tag;
  begin
    Boeing_777_Id := Ada.Tags.Internal_Tag("!@#$%^:*\/?"" not a tag!");
    Report.Failed ("Failed in Check_Exception, no exception");
    Boeing_777_Id := TC_ID_Tag( Boeing_777_Id ); 
  exception
    when Ada.Tags.Tag_Error => null;  
    when others =>
      Report.Failed ("Failed in Check_Exception, wrong exception");
  end Check_Exception;

  use Motivators;
  Two_Wheeler      : Bicycle;
  Four_Wheeler     : Car;
  Eighteen_Wheeler : Truck;

begin  -- Main test procedure.

    Report.Test ("C390002", "Check that a tagged type may be declared and " &
                 "derived from in simple, private and extended forms.  " &
                 "Check package Ada.Tags" );

    Create( Two_Wheeler,       2 );
    Create( Four_Wheeler,      4 );
    Create( Eighteen_Wheeler, 18 );
   
    Check_Tags( Machine       => Two_Wheeler, 
                Expected_Name => "C390002.MOTIVATORS.BICYCLE",
                External_Tag  => Bicycle'External_Tag );
    Check_Tags( Machine       => Four_Wheeler, 
                Expected_Name => "C390002.MOTIVATORS.CAR",
                External_Tag  => Car'External_Tag );
    Check_Tags( Machine       => Eighteen_Wheeler, 
                Expected_Name => "C390002.MOTIVATORS.TRUCK",
                External_Tag  => Truck'External_Tag );

    Check_Exception;
 
  Report.Result;

end C390002;
