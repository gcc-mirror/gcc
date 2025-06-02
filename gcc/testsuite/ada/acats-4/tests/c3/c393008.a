-- C393008.A
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
-- TEST OBJECTIVE:
--      Check that an extended type can be derived from an abstract type.
--
-- TEST DESCRIPTION:
--      Declare a tagged record; declare an abstract 
--      primitive operation and a non-abstract primitive operation of the 
--      type.  Derive an extended type from it, including a new component.
--      Use the derived type, the overriding operation and the inherited 
--      operation to instantiate a generic package.  The overriding operation
--      calls a new primitive operation and an inherited operation [so the 
--      instantiation must get this sorted out correctly].
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

with Report;
with TCTouch;
procedure C393008 is

package C393008_0 is

  type Status_Enum is (No_Status, Handled, Unhandled, Pending);

  type Alert_Type is abstract tagged record
      Status : Status_Enum;
      Reply  : Boolean;
      Urgent : Boolean;
  end record;

  subtype Serial_Number is Integer range 0..Integer'last;
  Serial_Num : Serial_Number := 0;

  procedure Handle   (A : in out Alert_Type) is abstract; 
                                        -- abstract primitive operation

  -- the procedure Init would be _nice_ have this procedure be non_abstract
  -- and create a "base" object with a "null" constraint.  The language
  -- will not allow this due to the restriction that an object of an
  -- abstract type cannot be created.  Hence Init must be abstract,
  -- requiring any type derived directly from Alert_Type to declare
  -- an Init.
  --
  -- In light of this, I have changed init to a function to more closely
  -- model the typical usage of OO features...

  function  Init return Alert_Type is abstract;

  procedure No_Reply (A : in out Alert_Type);

end C393008_0;

--=======================================================================--

package body C393008_0 is

  procedure No_Reply (A : in out Alert_Type) is
    begin                              -- primitive operation, not abstract
      TCTouch.Touch('A');  ------------------------------------------------- A
      if A.Status = Handled then
        A.Reply  := False;
      end if;
    end No_Reply;

end C393008_0;

--=======================================================================--

  generic
                        -- pass in the Alert_Type object, including its 
                        -- operations
    type Data_Type is new C393008_0.Alert_Type with private; 
                        -- note that Alert_Type is abstract, so it may not be
                        -- used as an actual parameter
    with procedure Update     (P : in out Data_Type) is <>;  -- generic formal
    with function  Initialize return Data_Type is <>;        -- generic formal

  package C393008_1 is
       -- Utilities

    procedure Modify (Item : in out Data_Type);

  end C393008_1;
   -- Utilities

--=======================================================================--

  package body C393008_1 is
            -- Utilities

      procedure Modify (Item : in out Data_Type) is
        begin
          TCTouch.Touch('B');  --------------------------------------------- B
          Item := Initialize;
          Update (Item);
        end Modify;

  end C393008_1;

--=======================================================================--

  package C393008_2 is

    type Low_Alert_Type is new C393008_0.Alert_Type with record
      Serial : C393008_0.Serial_Number;
    end record;
  
    procedure Serialize (LA : in out Low_Alert_Type);
  
    -- inherit No_Reply

    procedure Handle (LA : in out Low_Alert_Type);

    function Init return Low_Alert_Type;
  end C393008_2;

  package body C393008_2 is
    procedure Serialize (LA : in out Low_Alert_Type) is
    begin                          -- new primitive operation
      TCTouch.Touch('C');  ------------------------------------------------- C
      C393008_0.Serial_Num := C393008_0.Serial_Num + 1;
      LA.Serial := C393008_0.Serial_Num;
    end Serialize;
  
  -- inherit No_Reply

    function Init return Low_Alert_Type is
      TA: Low_Alert_Type;
    begin
      TCTouch.Touch('D');  ------------------------------------------------- D
      Serialize( TA );
      TA.Status := C393008_0.No_Status;
      return TA;
    end Init;

    procedure Handle (LA : in out Low_Alert_Type) is    
    begin                          -- overrides abstract inherited Handle
      TCTouch.Touch('E');  ------------------------------------------------- E
      Serialize (LA);
      LA.Reply := False;
      LA.Status := C393008_0.Handled;
      No_Reply (LA);
    end Handle;

  end C393008_2;

  use C393008_2;
  
  package Alert_Utilities is new
    C393008_1 (Data_Type   => Low_Alert_Type,
               Update      => Handle,   -- Low_Alert's Handle
               Initialize  => Init);    -- inherited from Alert

  Item : Low_Alert_Type;

  use type C393008_0.Status_Enum;

begin

  Report.Test ("C393008", "Check that an extended type can be derived "&
                          "from an abstract type");

  Item := Init;
  if (Item.Status /= C393008_0.No_Status) or (Item.Serial /=1)  then
    Report.Failed ("Wrong initialization");
  end if;
  TCTouch.Validate("DC", "Initialization Call");

  Alert_Utilities.Modify (Item);
  if (Item.Status /= C393008_0.Handled) or (Item.Serial /= 3) then 
    Report.Failed ("Wrong results from Modify");
  end if;
  TCTouch.Validate("BDCECA", "Generic Instance Call");

  Report.Result;

end C393008;
