-- C393009.A
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
--      Declare an abstract type in the specification of a generic package.
--      Instantiate the package and derive an extended type from the abstract
--      (instantiated) type; override all abstract operations; use all
--      inherited operations;
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      14 Oct 95   SAIC    Fixed for ACVC 2.0.1
--
--!
  
with Report;
procedure C393009 is
  
  package Display_Devices is
  
    type Display_Device_Enum is (None, TTY, Console, Big_Screen);
    Display : Display_Device_Enum := None;
  
  end Display_Devices;
  
--=======================================================================--
  
  generic
  
    type Generic_Status is (<>);
    
    type Serial_Type is (<>);
  
  package Alert_System is
  
    type Alert_Type (Serial : Serial_Type) is abstract tagged record
      Status : Generic_Status;
    end record;
    
    Next_Serial_Number : Serial_Type := Serial_Type'First;
  
    procedure Handle (A : in out Alert_Type) is abstract;
      -- abstract operation - must be overridden after instantiation
  
    procedure Display ( A : Alert_Type; 
                       On : Display_Devices.Display_Device_Enum);
              -- primitive operation of Alert_Type
              -- not required to be overridden
  
    function Get_Serial_Number (A : Alert_Type) return Serial_Type;
              -- primitive operation of Alert_Type
              -- not required to be overridden
  
  end Alert_System;
  
--=======================================================================--
  
  package body Alert_System is
  
    procedure Display ( A : in Alert_Type;
                       On : Display_Devices.Display_Device_Enum) is
      begin
        Display_Devices.Display := On;
      end Display;
  
    function Get_Serial_Number (A : Alert_Type) 
      return Serial_Type is
      begin
        return A.Serial;
      end Get_Serial_Number;
  
  end Alert_System;
  
--=======================================================================--

  package NCC_1701 is
  
    type Status_Kind is (Green, Yellow, Red);
    type Serial_Number_Type is new Integer range 1..Integer'Last;
    
    subtype Msg_Str is String (1..16);
    Alert_Msg : Msg_Str := "C393009  passed.";
                        --  123456789A123456
  
    package Alert_Pkg is new Alert_System (Status_Kind, Serial_Number_Type);
  
    type New_Alert_Type(Serial : Serial_Number_Type) is
      new Alert_Pkg.Alert_Type(Serial) with record
      Message : Msg_Str;
      end record;
   
    -- procedure Display is inherited by New_Alert_Type
  
    -- function Get_Serial_Number is inherited by New_Alert_Type
    procedure Handle (NA : in out New_Alert_Type);  -- must be overridden
    procedure Init   (NA : in out New_Alert_Type);  -- new primitive
  
    NA : New_Alert_Type(Alert_Pkg.Next_Serial_Number);
         -- New_Alert_Type is not abstract, so an object of that
         -- type may be declared
  
  end NCC_1701;
  
  package body NCC_1701 is
  
    procedure Handle (NA : in out New_Alert_Type) is
    begin
      NA.Message := Alert_Msg;
      Display (NA, On => Display_Devices.TTY);
      end Handle;
  
    procedure Init (NA : in out New_Alert_Type) is  -- new primitive operation
    begin                                           -- for New_Alert_Type
      NA := (Serial=> NA.Serial, Status => Green, Message => (others => ' '));
    end Init;
    
   end NCC_1701;
  
   use NCC_1701;
   use type Display_Devices.Display_Device_Enum;
  
begin

  Report.Test ("C393009", "Check that an extended type can be derived " &
                          "from an abstract type");

  Init (NA);
  if (Get_Serial_Number (NA) /= 1)
    or (NA.Status /= Green)
    or (Display_Devices.Display /= Display_Devices.None) then
      Report.Failed ("Wrong Initialization");
  end if;
  
  Handle (NA);
  if (Get_Serial_Number (NA) /= 1)
    or (NA.Status /= Green)
    or (NA.Message /= Alert_Msg) 
    or (Display_Devices.Display /= Display_Devices.TTY) then
      Report.Failed ("Wrong results from Handle");
  end if;
  
  Report.Result;

end C393009;
