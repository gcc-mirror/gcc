-- C360B01.A

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
--    Part 3: Arrays of real types.
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
-- CHANGE HISTORY:
--    30 Sep 2020 RLB Created test for RRS, donated to ACATS.

with Report, F360B00;
procedure C360B01 is

    use Report, F360B00;

    type My_Rec is record
        Flt : F360B00.Float_Array;
        Fix : F360B00.Fixed_Array;
        Dec : F360B00.Dec_Array;
    end record;

    type Float_Array_Array is array (1 .. 5) of F360B00.Float_Array;
    type Fix_Array_Array   is array (1 .. 5) of F360B00.Fixed_Array;
    type Dec_Array_Array   is array (1 .. 5) of F360B00.Dec_Array;

    function Array_Fix (X : in F360B00.Small_Fixed)
        return F360B00.Fixed_Array is
        -- Force a dynamic array value.
    begin
        if Equal (Ident_Int(1), 1) then
            return (1 .. 5 => X);
        else
            return (1 .. 5 => 0.0);
        end if;
    end Array_Fix;

    function Array_Flt (X : in F360B00.Small_Float)
        return F360B00.Float_Array is
        -- Force a dynamic array value.
    begin
        if Equal (Ident_Int(1), 1) then
            return (1 .. 5 => X);
        else
            return (1 .. 5 => 0.0);
        end if;
    end Array_Flt;

    function Array_Dec (X : in F360B00.Small_Dec)
        return F360B00.Dec_Array is
        -- Force a dynamic array value.
    begin
        if Equal (Ident_Int(1), 1) then
            return (1 .. 5 => X);
        else
            return (1 .. 5 => 0.0);
        end if;
    end Array_Dec;

begin
    Test ("C360B01", "Check that objects of types with a specified " &
                     "Default_Component_Value are appropriately " &
                     "initialized. Part 3 - Arrays of normal real " &
                     "types");

    --------------------------------------------------
    -- (A) Stand-alone objects:
    declare
        A_Flt : F360B00.Float_Array; -- Default initialized.
        B_Flt : F360B00.Float_Array := (1 .. 5 => 3.375);
        An_Int : F360B00.Fixed_Array; -- Default initialized.
        Bn_Int : F360B00.Fixed_Array := (1 .. 5 => 0.5);
        A_Word : F360B00.Dec_Array; -- Default initialized.
        B_Word : F360B00.Dec_Array := (1 .. 5 => 5.50);
    begin
        if A_Flt /= Array_Flt(Flt_Value) then
            Failed ("Bad default initialization of stand alone object of " &
                    "an array of a float type");
        end if;
        if B_Flt /= Array_Flt(3.375) then
            Failed ("Bad explicit initialization of stand alone object of " &
                    "an array of a float type");
        end if;
        if An_Int /= Array_Fix(Fix_Value) then
            Failed ("Bad default initialization of stand alone object of " &
                    "an array of a fixed type");
        end if;
        if Bn_Int /= Array_Fix(0.5) then
            Failed ("Bad explicit initialization of stand alone object of " &
                    "an array of a fixed type");
        end if;
        if A_Word /= Array_Dec(Dec_Value) then
            Failed ("Bad default initialization of stand alone object of " &
                    "an array of a decimal type");
        end if;
        if B_Word /= Array_Dec(5.50) then
            Failed ("Bad explicit initialization of stand alone object of " &
                    "an array of a decimal type");
        end if;
    end;

    --------------------------------------------------
    -- (B) Default initialized components:
    declare
        A_Rec : My_Rec;
        A_Arr : Float_Array_Array;
        B_Arr : Fix_Array_Array;
        C_Arr : Dec_Array_Array;
    begin
        if A_Rec.Flt /= Array_Flt(Flt_Value) then
            Failed ("Bad default initialization of record component of " &
                    "an array of a float type");
        end if;
        if A_Rec.Fix /= Array_Fix(Fix_Value) then
            Failed ("Bad default initialization of record component of " &
                    "an array of a fixed type");
        end if;
        if A_Rec.Dec /= Array_Dec(Dec_Value) then
            Failed ("Bad default initialization of record component of " &
                    "an array of a decimal type");
        end if;
        if A_Arr /= (1 .. 5 => Array_Flt(Flt_Value)) then
            Failed ("Bad default initialization of array component of " &
                    "an array of a float type");
        end if;
        if B_Arr /= (1 .. 5 => Array_Fix(Fix_Value)) then
            Failed ("Bad default initialization of array component of " &
                    "an array of a fixed type");
        end if;
        if C_Arr /= (1 .. 5 => Array_Dec(Dec_Value)) then
            Failed ("Bad default initialization of array component of " &
                    "an array of a decimal type");
        end if;
    end;

    --------------------------------------------------
    -- (C) <> initialization in array and record aggregates:
    declare
        A_Rec : My_Rec := (Flt => <>, Fix => (1 .. 5 => 2.0),
                           Dec => (1 .. 5 => 0.0));
        B_Rec : My_Rec := (Flt => (1 .. 5 => 1.25), Fix => <>,
                           Dec => (1 .. 5 => 0.0));
        C_Rec : My_Rec := (Flt => (1 .. 5 => 1.25), Fix => (1 .. 5 => 2.0),
                           Dec => <>);
        A_Arr : Float_Array_Array := (1 => (1 .. 5 => -1.0), 2 .. 5 => <>);
        B_Arr : Fix_Array_Array := (1 => (1 .. 5 =>  0.0), 2 => <>,
                                    3 => (1 .. 5 => -2.0), 4 .. 5 => <>);
        C_Arr : Dec_Array_Array := (1|3|5 => <>, 2 => (1 .. 5 => 3.50),
                                    4 => (1 .. 5 => 7.52));
    begin
        if A_Rec /= (Flt => Array_Flt(Flt_Value),
                     Fix => Array_Fix(2.0),
                     Dec => Array_Dec(0.0)) then
            Failed ("Bad default initialization of aggregate record " &
                    "component of an array of a float type");
        end if;
        if B_Rec /= (Flt => Array_Flt(1.25),
                     Fix => Array_Fix(Fix_Value),
                     Dec => Array_Dec(0.0)) then
            Failed ("Bad default initialization of aggregate record " &
                    "component of an array of a fixed type");
        end if;
        if C_Rec /= (Flt => Array_Flt(1.25),
                     Fix => Array_Fix(2.0),
                     Dec => Array_Dec(Dec_Value)) then
            Failed ("Bad default initialization of aggregate record " &
                    "component of an array of a decimal type");
        end if;
        if A_Arr /= (1 => Array_Flt(-1.0),
                     2 .. 5 => Array_Flt(Flt_Value)) then
            Failed ("Bad default initialization of aggregate array " &
                    "component of an array of a float type");
        end if;
        if B_Arr /= (1 => Array_Fix(0.0), 3 => Array_Fix(-2.0),
                     2 | 4 .. 5 => Array_Fix(Fix_Value)) then
            Failed ("Bad default initialization of aggregate array " &
                    "component of an array of a fixed type");
        end if;
        if C_Arr /= (2 => Array_Dec(3.50), 4 => Array_Dec(7.52),
                     1 | 3 | 5 => Array_Dec(Dec_Value)) then
            Failed ("Bad default initialization of aggregate array " &
                    "component of an array of a decimal type");
        end if;
    end;

    --------------------------------------------------
    -- (D) Allocators:
    --------------------------------------------------
    declare
        type Flt_Access is access F360B00.Float_Array;
        F_Ptr : Flt_Access := new F360B00.Float_Array; -- Default initialized.

        type Fix_Access is access F360B00.Fixed_Array;
        X_Ptr : Fix_Access := new F360B00.Fixed_Array; -- Default initialized.

        type Dec_Access is access F360B00.Dec_Array;
        D_Ptr : Dec_Access := new F360B00.Dec_Array; -- Default initialized.

        type Rec_Access is access My_Rec;
        R_Ptr : Rec_Access := new My_Rec; -- Default initialized.

        type Arr_Access is access Float_Array_Array;
        A_Ptr : Arr_Access := new Float_Array_Array; -- Default initialized.
    begin
        if F_Ptr.all /= Array_Flt(Flt_Value) then
            Failed ("Bad default initialization of allocator of " &
                    "an array of a float type");
        end if;
        if X_Ptr.all /= Array_Fix(Fix_Value) then
            Failed ("Bad default initialization of allocator of " &
                    "an array of a fixed type");
        end if;
        if D_Ptr.all /= Array_Dec(Dec_Value) then
            Failed ("Bad default initialization of allocator of " &
                    "an array of a decimal type");
        end if;
        if R_Ptr.Flt /= Array_Flt(Flt_Value) then
            Failed ("Bad default initialization of record component of " &
                    "allocator of an array of a float type");
        end if;
        if R_Ptr.Fix /= Array_Fix(Fix_Value) then
            Failed ("Bad default initialization of record component of " &
                    "allocator of an array of a fixed type");
        end if;
        if R_Ptr.Dec /= Array_Dec(Dec_Value) then
            Failed ("Bad default initialization of record component of " &
                    "allocator of an array of a decimal type");
        end if;
        if A_Ptr.all /= (1 .. 5 => Array_Flt(Flt_Value)) then
            Failed ("Bad default initialization of array component of " &
                    "allocator of an array of a float type");
        end if;
    end;

    Result;

end C360B01;
