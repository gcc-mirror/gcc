-- C940015.A
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
--      Check that the component_declarations of a protected_operation
--      are elaborated in the proper order.
--
-- TEST DESCRIPTION:
--      A discriminated protected object is declared with some
--      components that depend upon the discriminant and some that
--      do not depend upon the discriminant.  All the components
--      are initialized with a function call.  As a side-effect of
--      the function call the parameter passed to the function is
--      recorded in an elaboration order array.  
--      Two objects of the protected type are declared.  The 
--      elaboration order is recorded and checked against the
--      expected order.
--
--
-- CHANGE HISTORY:
--      09 Jan 96   SAIC    Initial Version for 2.1
--      09 Jul 96   SAIC    Addressed reviewer comments.
--      13 Feb 97   PWB.CTA Removed doomed attempt to check per-object
--                          constraint elaborations.
--!


with Report;

procedure C940015 is
    Verbose : constant Boolean := False;
    Do_Display : Boolean := Verbose;
     
    type Index is range 0..10;

    type List is array (1..10) of Integer;
    Last : Natural range 0 .. List'Last := 0;
    E_List : List := (others => 0);

    function Elaborate (Id : Integer) return Index is
    begin
        Last := Last + 1;
        E_List (Last) := Id;
        if Verbose then
            Report.Comment ("Elaborating" & Integer'Image (Id));
        end if;
        return Index(Id mod 10);
    end Elaborate;

    function Elaborate (Id, Per_Obj_Expr : Integer) return Index is
    begin
        return Elaborate (Id);
    end Elaborate;

begin
 
    Report.Test ("C940015", "Check that the component_declarations of a" &
                            " protected object are elaborated in the" &
                            " proper order");
    declare
        -- an unprotected queue type
        type Storage is array (Index range <>) of Integer;
        type Queue (Size, Flag : Index := 1) is
            record
                Head : Index := 1;
                Tail : Index := 1;
                Count : Index := 0;
                Buffer : Storage (1..Size);
            end record;

        -- protected group of queues type
        protected type Prot_Queues (Size : Index := Elaborate (104)) is 
            procedure Clear;
            -- other needed procedures not provided at this time
        private
               -- elaborate at type elaboration
            Fixed_Queue_1    : Queue (3,  
                                      Elaborate (105));
               -- elaborate at type elaboration
            Fixed_Queue_2    : Queue (6,    
                                      Elaborate (107));
        end Prot_Queues;
        protected body Prot_Queues is
            procedure Clear is
            begin 
                Fixed_Queue_1.Count := 0;
                Fixed_Queue_1.Head := 1;
                Fixed_Queue_1.Tail := 1;
                Fixed_Queue_2.Count := 0;
                Fixed_Queue_2.Head := 1;
                Fixed_Queue_2.Tail := 1;
            end Clear;
        end Prot_Queues;
          
        PO1 : Prot_Queues(9);
        PO2 : Prot_Queues;

        Expected_Elab_Order : List := (
           -- from the elaboration of the protected type Prot_Queues
           105, 107,
           -- from the unconstrained object PO2
           104,
           others => 0);
    begin
        for I in List'Range loop
            if E_List (I) /= Expected_Elab_Order (I) then
                Report.Failed ("wrong elaboration order"); 
                Do_Display := True;
            end if;
        end loop;
        if Do_Display then
            Report.Comment ("Expected  Actual");
            for I in List'Range loop
                Report.Comment (
                   Integer'Image (Expected_Elab_Order(I)) &
                   Integer'Image (E_List(I)));
            end loop;
        end if;

        -- make use of the protected objects
        PO1.Clear;
        PO2.Clear;
    end;

    Report.Result;
 
end C940015;
