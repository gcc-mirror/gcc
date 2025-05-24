-- C360A01.A

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
-- OBJECTIVE:
--    Check that objects of types with a specified Default_Component_Value are
--    appropriately initialized.
--
--    Part 1: Arrays of discrete types.
--
-- TEST DESCRIPTION:
--    We assume that if a programmer cares to define a Default_Component_Value
--    for a type, they care that objects of the type are properly initialized
--    in any possible context. Thus, we do not attempt to define a particular
--    usage scenario.
--
--    The test types are declared in a foundation package in order to model a
--    more realistic usage.
--
--    We test for initialization in the following contexts:
--       (A) Stand-alone objects;
--       (B) Default initialized components;
--       (C) <> initialization in array and record aggregates;
--       (D) Allocators.
--
--    We also test that default values out of range raise Constraint_Error.
--
-- CHANGE HISTORY:
--    30 Sep 2020 RLB Created test for RRS, donated to ACATS.

with Report, F360A00;
procedure C360A01 is

    use Report, F360A00;

    type My_Rec is record
        Kind : F360A00.Status_Array;
        Num  : F360A00.Int_Array;
        Cnt  : F360A00.Word_Array;
    end record;

    type Status_Array_Array  is array (1 .. 5) of F360A00.Status_Array;
    type Int_Array_Array     is array (1 .. 5) of F360A00.Int_Array;
    type Word_Array_Array    is array (1 .. 5) of F360A00.Word_Array;

    function Array_Int (X : in F360A00.Small_Integer)
        return F360A00.Int_Array is
        -- Force an array dynamic value.
    begin
        return (1 .. 5 => F360A00.Small_Integer(Report.Ident_Int(Integer(X))));
    end Array_Int;

    function Array_Status (X : in F360A00.Status)
        return F360A00.Status_Array is
        -- Force an array dynamic value.
    begin
        return (1 ..5 =>
                  F360A00.Status'Val(Report.Ident_Int(F360A00.Status'Pos(X))));
    end Array_Status;

    function Array_Word (X : in F360A00.Word_16)
        return F360A00.Word_Array is
        -- Force an array dynamic value.
    begin
        if Equal (Ident_Int(1), 1) then
            return (1 .. 5 => X);
        else
            return (1 .. 5 => 0);
        end if;
    end Array_Word;

    -- Note: We can't safely write a Array_Word that directly uses Ident_Int
    -- as the range of Integer might not include Word_Array'Last.

 begin
    Test ("C360A01", "Check that objects of types with a specified " &
                     "Default_Component_Value are appropriately " &
                     "initialized. Part 1 - Arrays of normal discrete " &
                     "types");

    --------------------------------------------------
    -- (A) Stand-alone objects:
    declare
        A_Kind : F360A00.Status_Array; -- Default initialized.
        B_Kind : F360A00.Status_Array := (1 .. 5 => Raw);
        An_Int : F360A00.Int_Array; -- Default initialized.
        Bn_Int : F360A00.Int_Array := (1 .. 5 => 0);
        A_Word : F360A00.Word_Array; -- Default initialized.
        B_Word : F360A00.Word_Array := (1 .. 5 => 16#FFFF#);
    begin
        if A_Kind /= Array_Status(Unknown) then
            Failed ("Bad default initialization of stand alone object of " &
                    "an array of an enumeration type");
        end if;
        if B_Kind /= Array_Status(Raw) then
            Failed ("Bad explicit initialization of stand alone object of " &
                    "an array of an enumeration type");
        end if;
        if An_Int /= Array_Int(Unused) then
            Failed ("Bad default initialization of stand alone object of " &
                    "an array of an integer type");
        end if;
        if Bn_Int /= Array_Int(0) then
            Failed ("Bad explicit initialization of stand alone object of " &
                    "an array of an integer type");
        end if;
        if A_Word /= Array_Word(16#DEAD#) then
            Failed ("Bad default initialization of stand alone object of " &
                    "an array of a modular type");
        end if;
        if B_Word /= Array_Word(16#FFFF#) then
            Failed ("Bad explicit initialization of stand alone object of " &
                    "an array of a modular type");
        end if;
    end;

    --------------------------------------------------
    -- (B) Default initialized components:
    declare
        A_Rec : My_Rec;
        A_Arr : Status_Array_Array;
        B_Arr : Int_Array_Array;
        C_Arr : Word_Array_Array;
    begin
        if A_Rec.Kind /= Array_Status(Unknown) then
            Failed ("Bad default initialization of record component of " &
                    "an array of an enumeration type");
        end if;
        if A_Rec.Num /= Array_Int(Unused) then
            Failed ("Bad default initialization of record component of " &
                    "an array of an integer type");
        end if;
        if A_Rec.Cnt /= Array_Word(16#DEAD#) then
            Failed ("Bad default initialization of record component of " &
                    "an array of a modular type");
        end if;
        if A_Arr /= (1 .. 5 => Array_Status(Unknown)) then
            Failed ("Bad default initialization of array component of " &
                    "an array of an enumeration type");
        end if;
        if B_Arr /= (1 .. 5 => Array_Int(Unused)) then
            Failed ("Bad default initialization of array component of " &
                    "an array of an integer type");
        end if;
        if C_Arr /= (1 .. 5 => Array_Word(16#DEAD#)) then
            Failed ("Bad default initialization of array component of " &
                    "an array of a modular type");
        end if;
    end;

    --------------------------------------------------
    -- (C) <> initialization in array and record aggregates:
    declare
        A_Rec : My_Rec := (Kind => <>,
                           Num => (1 ..5 => 12), Cnt => (1 .. 5 => 0));
        B_Rec : My_Rec := (Kind => (1 .. 5 => Raw),
                           Num => <>, Cnt => (1 .. 5 => 0));
        C_Rec : My_Rec := (Kind => (1 .. 5 => Raw),
                           Num => (1 .. 5 => 12), Cnt => <>);
        A_Arr : Status_Array_Array := (1 => (1 .. 5 => Solved), 2 .. 5 => <>);
        B_Arr : Int_Array_Array := (1 => (1 .. 5 => 0), 2 => <>,
                                    3 => (1 .. 5 => -4), 4 .. 5 => <>);
        C_Arr : Word_Array_Array := (1|3|5 => <>, 2 => (1 .. 5 => 0),
                                     4 => (1 .. 5 => 52));
    begin
        if A_Rec /= (Kind => Array_Status(Unknown),
                     Num  => Array_Int(12), Cnt => (1 .. 5 => 0)) then
            Failed ("Bad default initialization of aggregate record " &
                    "component of an array of an enumeration type");
        end if;
        if B_Rec /= (Kind => Array_Status(Raw),
                     Num  => Array_Int(Unused), Cnt => (1 .. 5 => 0)) then
            Failed ("Bad default initialization of aggregate record " &
                    "component of an array of an integer type");
        end if;
        if C_Rec /= (Kind => Array_Status(Raw),
                     Num  => Array_Int(12), Cnt => (1 .. 5 => 16#DEAD#)) then
            Failed ("Bad default initialization of aggregate record " &
                    "component of an array of a modular type");
        end if;
        if A_Arr /= (1 => Array_Status(Solved),
                     2 .. 5 => Array_Status(Unknown)) then
            Failed ("Bad default initialization of aggregate array " &
                    "component of an array of an enumeration type");
        end if;
        if B_Arr /= (1 => Array_Int(0), 3 => Array_Int(-4),
                     2 | 4 .. 5 => Array_Int(Unused)) then
            Failed ("Bad default initialization of aggregate array " &
                    "component of an array of an integer type");
        end if;
        if C_Arr /= (2 => Array_Word(0), 4 => Array_Word(52),
                     1 | 3 | 5 => Array_Word(16#DEAD#)) then
            Failed ("Bad default initialization of aggregate array " &
                    "component of an array of a modular type");
        end if;
    end;

    --------------------------------------------------
    -- (D) Allocators:
    --------------------------------------------------
    declare
        type Enum_Access is access F360A00.Status_Array;
        E_Ptr : Enum_Access := new F360A00.Status_Array; -- Default initialized.

        type Int_Access is access F360A00.Int_Array;
        I_Ptr : Int_Access := new F360A00.Int_Array;-- Default initialized.

        type Mod_Access is access F360A00.Word_Array;
        M_Ptr : Mod_Access := new F360A00.Word_Array; -- Default initialized.

        type Rec_Access is access My_Rec;
        R_Ptr : Rec_Access := new My_Rec; -- Default initialized.

        type Arr_Access is access Status_Array_Array;
        A_Ptr : Arr_Access := new Status_Array_Array; -- Default initialized.
    begin
        if E_Ptr.all /= Array_Status(Unknown) then
            Failed ("Bad default initialization of allocator of " &
                    "an array of an enumeration type");
        end if;
        if I_Ptr.all /= Array_Int(Unused) then
            Failed ("Bad default initialization of allocator of " &
                    "an array of an integer type");
        end if;
        if M_Ptr.all /= Array_Word(16#DEAD#) then
            Failed ("Bad default initialization of allocator of " &
                    "an array of a modular type");
        end if;
        if R_Ptr.Kind /= Array_Status(Unknown) then
            Failed ("Bad default initialization of record component of " &
                    "allocator of an array of an enumeration type");
        end if;
        if R_Ptr.Num /= Array_Int(Unused) then
            Failed ("Bad default initialization of record component of " &
                    "allocator of an array of an integer type");
        end if;
        if R_Ptr.Cnt /= Array_Word(16#DEAD#) then
            Failed ("Bad default initialization of record component of " &
                    "allocator of an array of a modular type");
        end if;
        if A_Ptr.all /= (1 .. 5 => Array_Status(Unknown)) then
            Failed ("Bad default initialization of array component of " &
                    "allocator of an array of an enumeration type");
        end if;
    end;

    Result;

end C360A01;
