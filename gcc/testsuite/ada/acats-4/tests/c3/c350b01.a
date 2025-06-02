-- C350B01.A

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
--    Check that objects of types with a specified Default_Value are
--    appropriately initialized.
--
--    Part 3: Normal real types.
--
-- TEST DESCRIPTION:
--    We assume that if a programmer cares to define a Default_Value for a
--    type, they care that objects of the type are properly initialized in any
--    possible context. Thus, we do not attempt to define a particular usage
--    scenario.
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
--    30 Jun 2020 RLB Created test for RRS, donated to ACATS.

with Report, F350B00;
procedure C350B01 is

    use Report, F350B00;

    type My_Rec is record
        Flt : F350B00.Small_Float;
        Fix : F350B00.Small_Fixed;
        Dec : F350B00.Small_Dec;
    end record;

    type Sml_Float_Array is array (1 .. 5) of F350B00.Small_Float;
    type Sml_Fix_Array   is array (1 .. 5) of F350B00.Small_Fixed;
    type Sml_Dec_Array   is array (1 .. 5) of F350B00.Small_Dec;

    function Ident_SFix (X : in F350B00.Small_Fixed)
        return F350B00.Small_Fixed is
        -- Force a dynamic value.
    begin
        if Equal (Ident_Int(1), 1) then
            return X;
        else
            return 0.0;
        end if;
    end Ident_SFix;

    function Ident_SFloat (X : in F350B00.Small_Float)
        return F350B00.Small_Float is
        -- Force a dynamic value.
    begin
        if Equal (Ident_Int(1), 1) then
            return X;
        else
            return 0.0;
        end if;
    end Ident_SFloat;

    function Ident_SDec (X : in F350B00.Small_Dec)
        return F350B00.Small_Dec is
        -- Force a dynamic value.
    begin
        if Equal (Ident_Int(1), 1) then
            return X;
        else
            return 0.0;
        end if;
    end Ident_SDec;

begin
    Test ("C350B01", "Check that objects of types with a specified " &
                     "Default_Value are appropriately initialized. " &
                     "Part 3 - Normal real types");

    --------------------------------------------------
    -- (A) Stand-alone objects:
    declare
        A_Flt : F350B00.Small_Float; -- Default initialized.
        B_Flt : F350B00.Small_Float := 3.375;
        An_Int : F350B00.Small_Fixed; -- Default initialized.
        Bn_Int : F350B00.Small_Fixed := 0.5;
        A_Word : F350B00.Small_Dec; -- Default initialized.
        B_Word : F350B00.Small_Dec := 5.50;
    begin
        if A_Flt /= Ident_SFloat(Flt_Value) then
            Failed ("Bad default initialization of stand alone object of " &
                    "a float type");
        end if;
        if B_Flt /= Ident_SFloat(3.375) then
            Failed ("Bad explicit initialization of stand alone object of " &
                    "a float type");
        end if;
        if An_Int /= Ident_SFix(Fix_Value) then
            Failed ("Bad default initialization of stand alone object of " &
                    "a fixed type");
        end if;
        if Bn_Int /= Ident_SFix(0.5) then
            Failed ("Bad explicit initialization of stand alone object of " &
                    "a fixed type");
        end if;
        if A_Word /= Ident_SDec(Dec_Value) then
            Failed ("Bad default initialization of stand alone object of " &
                    "a decimal type");
        end if;
        if B_Word /= Ident_SDec(5.50) then
            Failed ("Bad explicit initialization of stand alone object of " &
                    "a decimal type");
        end if;
    end;

    --------------------------------------------------
    -- (B) Default initialized components:
    declare
        A_Rec : My_Rec;
        A_Arr : Sml_Float_Array;
        B_Arr : Sml_Fix_Array;
        C_Arr : Sml_Dec_Array;
    begin
        if A_Rec.Flt /= Ident_SFloat(Flt_Value) then
            Failed ("Bad default initialization of record component of " &
                    "a float type");
        end if;
        if A_Rec.Fix /= Ident_SFix(Fix_Value) then
            Failed ("Bad default initialization of record component of " &
                    "a fixed type");
        end if;
        if A_Rec.Dec /= Ident_SDec(Dec_Value) then
            Failed ("Bad default initialization of record component of " &
                    "a decimal type");
        end if;
        if A_Arr /= (1 .. 5 => Ident_SFloat(Flt_Value)) then
            Failed ("Bad default initialization of array component of " &
                    "a float type");
        end if;
        if B_Arr /= (1 .. 5 => Ident_SFix(Fix_Value)) then
            Failed ("Bad default initialization of array component of " &
                    "a fixed type");
        end if;
        if C_Arr /= (1 .. 5 => Dec_Value) then
            Failed ("Bad default initialization of array component of " &
                    "a decimal type");
        end if;
    end;

    --------------------------------------------------
    -- (C) <> initialization in array and record aggregates:
    declare
        A_Rec : My_Rec := (Flt => <>, Fix => 2.0, Dec => 0.0);
        B_Rec : My_Rec := (Flt => 1.25, Fix => <>, Dec => 0.0);
        C_Rec : My_Rec := (Flt => 1.25, Fix => 2.0, Dec => <>);
        A_Arr : Sml_Float_Array := (1 => -1.0, 2 .. 5 => <>);
        B_Arr : Sml_Fix_Array := (1 => 0.0, 2 => <>, 3 => -2.0, 4 .. 5 => <>);
        C_Arr : Sml_Dec_Array := (1|3|5 => <>, 2 => 3.50, 4 => 7.52);
    begin
        if A_Rec /= (Flt => Ident_SFloat(Flt_Value),
                     Fix => Ident_SFix(2.0),
                     Dec => Ident_SDec(0.0)) then
            Failed ("Bad default initialization of aggregate record " &
                    "component of a float type");
        end if;
        if B_Rec /= (Flt => Ident_SFloat(1.25),
                     Fix => Ident_SFix(Fix_Value),
                     Dec => Ident_SDec(0.0)) then
            Failed ("Bad default initialization of aggregate record " &
                    "component of a fixed type");
        end if;
        if C_Rec /= (Flt => Ident_SFloat(1.25),
                     Fix => Ident_SFix(2.0),
                     Dec => Ident_SDec(Dec_Value)) then
            Failed ("Bad default initialization of aggregate record " &
                    "component of a decimal type");
        end if;
        if A_Arr /= (1 => Ident_SFloat(-1.0),
                     2 .. 5 => Ident_SFloat(Flt_Value)) then
            Failed ("Bad default initialization of aggregate array " &
                    "component of a float type");
        end if;
        if B_Arr /= (1 => Ident_SFix(0.0), 3 => Ident_SFix(-2.0),
                     2 | 4 .. 5 => Ident_SFix(Fix_Value)) then
            Failed ("Bad default initialization of aggregate array " &
                    "component of a fixed type");
        end if;
        if C_Arr /= (2 => 3.50, 4 => 7.52,
                     1 | 3 | 5 => Ident_SDec(Dec_Value)) then
            Failed ("Bad default initialization of aggregate array " &
                    "component of a decimal type");
        end if;
    end;

    --------------------------------------------------
    -- (D) Allocators:
    --------------------------------------------------
    declare
        type Flt_Access is access F350B00.Small_Float;
        F_Ptr : Flt_Access := new F350B00.Small_Float; -- Default initialized.

        type Fix_Access is access F350B00.Small_Fixed;
        X_Ptr : Fix_Access := new F350B00.Small_Fixed; -- Default initialized.

        type Dec_Access is access F350B00.Small_Dec;
        D_Ptr : Dec_Access := new F350B00.Small_Dec; -- Default initialized.

        type Rec_Access is access My_Rec;
        R_Ptr : Rec_Access := new My_Rec; -- Default initialized.

        type Arr_Access is access Sml_Float_Array;
        A_Ptr : Arr_Access := new Sml_Float_Array; -- Default initialized.
    begin
        if F_Ptr.all /= Ident_SFloat(Flt_Value) then
            Failed ("Bad default initialization of allocator of " &
                    "a float type");
        end if;
        if X_Ptr.all /= Ident_SFix(Fix_Value) then
            Failed ("Bad default initialization of allocator of " &
                    "a fixed type");
        end if;
        if D_Ptr.all /= Ident_SDec(Dec_Value) then
            Failed ("Bad default initialization of allocator of " &
                    "a decimal type");
        end if;
        if R_Ptr.Flt /= Ident_SFloat(Flt_Value) then
            Failed ("Bad default initialization of record component of " &
                    "allocator of a float type");
        end if;
        if R_Ptr.Fix /= Ident_SFix(Fix_Value) then
            Failed ("Bad default initialization of record component of " &
                    "allocator of a fixed type");
        end if;
        if R_Ptr.Dec /= Ident_SDec(Dec_Value) then
            Failed ("Bad default initialization of record component of " &
                    "allocator of a decimal type");
        end if;
        if A_Ptr.all /= (1 .. 5 => Ident_SFloat(Flt_Value)) then
            Failed ("Bad default initialization of array component of " &
                    "allocator of a float type");
        end if;
    end;

    Result;

end C350B01;
