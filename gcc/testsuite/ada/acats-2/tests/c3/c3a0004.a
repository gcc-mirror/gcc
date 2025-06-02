-- C3A0004.A
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
 --      Check that access to subprogram may be stored within array 
 --      objects, and that the access to subprogram can subsequently 
 --      be called. 
 -- 
 -- TEST DESCRIPTION:
 --      Declare an access to procedure type in a package specification.  
 --      Declare an array of the access type.  Declare three different 
 --      procedures that can be referred to by the access to procedure type.  
 --
 --      In the main program, build the array by dereferencing the access 
 --      value.
 --
 --
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
 --!
 
 with Report;
 
 procedure C3A0004 is
 
    Left_Turn   : Integer  := 1;
 
    Right_Turn  : Integer  := 1;
 
    Center_Turn : Integer  := 1;
 
    -- Type accesses to any procedure
    type Action_Ptr is access procedure;
    
    -- Array of access to procedure
    type Action_Array is array (Integer range <>) of Action_Ptr;
 
 
    procedure Rotate_Left is
    begin
       Left_Turn := 2;
    end Rotate_Left;
 
 
    procedure Rotate_Right is
    begin
       Right_Turn := 3;
    end Rotate_Right;
 
 
    procedure Center is
    begin
       Center_Turn := 0;
    end Center;
 
 
 begin
 
    Report.Test ("C3A0004", "Check that access to subprogram may be "
                          & "stored within data structures, and that the "
                          & "access to subprogram can subsequently be called");
 
    ------------------------------------------------------------------------
 
    declare
       Total_Actions   : constant := 3;
       Action_Sequence : Action_Array (1 .. Total_Actions);
 
    begin
 
       -- Build the action sequence
       Action_Sequence := (Rotate_Left'Access, Center'Access,
                           Rotate_Right'Access);
 
       -- Assign actions by invoking subprogram designated by access value
       for I in Action_Sequence'Range loop
          Action_Sequence(I).all;
       end loop;
 
       If Left_Turn /= 2 or Right_Turn /= 3
          or Center_Turn /= 0 then
             Report.Failed ("Incorrect Action sequence result");
       end if;
 
     end;
 
    ------------------------------------------------------------------------
 
    Report.Result;
 
 end C3A0004;
 
