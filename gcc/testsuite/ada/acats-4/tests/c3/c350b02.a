-- C350B02.A

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
--    Part 4: Generic real types.
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
procedure C350B02 is

    use Report, F350B00;

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


    generic
        type GFlt is digits <>;
        Default : in GFlt; -- The default value.
        Test : in String;
    procedure Check_Flt;

    procedure Check_Flt is
        type My_Rec is record
            Num  : GFlt;
            Cnt  : Natural;
        end record;
        type G_Array is array (1 .. 5) of GFlt;
    begin
        -- (A) Stand-alone objects:
        declare
            An_Obj : GFlt; -- Default initialized.
            Bn_Obj : GFlt := 3.0;
        begin
            if An_Obj /= Default then
                Failed ("Bad default initialization of stand alone object " &
                        "of a float type - " & Test);
            end if;
            if Bn_Obj /= 3.0 then
                Failed ("Bad explicit initialization of stand alone object " &
                        "of a float type - " & Test);
            end if;
        end;
        --------------------------------------------------
        -- (B) Default initialized components:
        declare
            A_Rec : My_Rec;
            A_Arr : G_Array;
        begin
            if A_Rec.Num /= Default then
                Failed ("Bad default initialization of record component of " &
                        "a float type - " & Test);
            end if;
            if A_Arr /= (1 .. 5 => Default) then
                Failed ("Bad default initialization of array component of " &
                        "a float type - " & Test);
            end if;
        end;
        --------------------------------------------------
        -- (C) <> initialization in array and record aggregates:
        declare
            A_Rec : My_Rec := (Num => <>, Cnt => 12);
            B_Rec : My_Rec := (Num => 2.5, Cnt => 0);
            A_Arr : G_Array := (1 => 2.5, 2 .. 5 => <>);
        begin
            if A_Rec /= (Num  => Default, Cnt => 12) then
                Failed ("Bad default initialization of aggregate record " &
                        "component of a float type - " & Test);
            end if;
            if B_Rec /= (Num  => 2.5, Cnt => 0) then
                Failed ("Bad explicit initialization of aggregate record " &
                        "component of a float type - " & Test);
            end if;
            if A_Arr /= (1 => 2.5, 2 .. 5 => Default) then
                Failed ("Bad default initialization of aggregate array " &
                        "component of a float type - " & Test);
            end if;
        end;

        --------------------------------------------------
        -- (D) Allocators:
        --------------------------------------------------
        declare
            type G_Access is access GFlt;
            G_Ptr : G_Access := new GFlt; -- Default initialized.

            type Rec_Access is access My_Rec;
            R_Ptr : Rec_Access := new My_Rec; -- Default initialized.

            type Arr_Access is access G_Array;
            A_Ptr : Arr_Access := new G_Array; -- Default initialized.
        begin
            if G_Ptr.all /= Default then
                Failed ("Bad default initialization of allocator of " &
                        "component of a float type - " & Test);
            end if;
            if R_Ptr.Num /= Default then
                Failed ("Bad default initialization of record component of " &
                        "allocator of a float type - " & Test);
            end if;
            if A_Ptr.all /= (1 .. 5 => Default) then
                Failed ("Bad default initialization of array component of " &
                        "allocator of a float type - " & Test);
            end if;
        end;
    end Check_Flt;


    generic
        type GFix is delta <>;
        Default : in GFix; -- The default value.
        Other_Val : in GFix; -- Some other value.
        Test : in String;
    procedure Check_Fix;

    procedure Check_Fix is
        type My_Rec is record
            Num  : GFix;
            Cnt  : Natural;
        end record;
        type G_Array is array (1 .. 5) of GFix;
    begin
        -- (A) Stand-alone objects:
        declare
            An_Obj : GFix; -- Default initialized.
            Bn_Obj : GFix := Other_Val;
        begin
            if An_Obj /= Default then
                Failed ("Bad default initialization of stand alone object " &
                        "of a fixed type - " & Test);
            end if;
            if Bn_Obj /= Other_Val then
                Failed ("Bad explicit initialization of stand alone object " &
                        "of a fixed type - " & Test);
            end if;
        end;
        --------------------------------------------------
        -- (B) Default initialized components:
        declare
            A_Rec : My_Rec;
            A_Arr : G_Array;
        begin
            if A_Rec.Num /= Default then
                Failed ("Bad default initialization of record component of " &
                        "a fixed type - " & Test);
            end if;
            if A_Arr /= (1 .. 5 => Default) then
                Failed ("Bad default initialization of array component of " &
                        "a fixed type - " & Test);
            end if;
        end;
        --------------------------------------------------
        -- (C) <> initialization in array and record aggregates:
        declare
            A_Rec : My_Rec := (Num => <>, Cnt => 12);
            B_Rec : My_Rec := (Num => Other_Val, Cnt => 0);
            A_Arr : G_Array := (1 => Other_Val, 2 .. 5 => <>);
        begin
            if A_Rec /= (Num  => Default, Cnt => 12) then
                Failed ("Bad default initialization of aggregate record " &
                        "component of a fixed type - " & Test);
            end if;
            if B_Rec /= (Num  => Other_Val, Cnt => 0) then
                Failed ("Bad explicit initialization of aggregate record " &
                        "component of a fixed type - " & Test);
            end if;
            if A_Arr /= (1 => Other_Val, 2 .. 5 => Default) then
                Failed ("Bad default initialization of aggregate array " &
                        "component of a fixed type - " & Test);
            end if;
        end;

        --------------------------------------------------
        -- (D) Allocators:
        --------------------------------------------------
        declare
            type G_Access is access GFix;
            G_Ptr : G_Access := new GFix; -- Default initialized.

            type Rec_Access is access My_Rec;
            R_Ptr : Rec_Access := new My_Rec; -- Default initialized.

            type Arr_Access is access G_Array;
            A_Ptr : Arr_Access := new G_Array; -- Default initialized.
        begin
            if G_Ptr.all /= Default then
                Failed ("Bad default initialization of allocator of " &
                        "component of a fixed type - " & Test);
            end if;
            if R_Ptr.Num /= Default then
                Failed ("Bad default initialization of record component of " &
                        "allocator of a fixed type - " & Test);
            end if;
            if A_Ptr.all /= (1 .. 5 => Default) then
                Failed ("Bad default initialization of array component of " &
                        "allocator of a fixed type - " & Test);
            end if;
        end;
    end Check_Fix;


    generic
        type GDec is delta <> digits <>;
        Default : in GDec; -- The default value.
        Test : in String;
    procedure Check_Dec;

    procedure Check_Dec is
        type My_Rec is record
            Num  : GDec;
            Cnt  : Natural;
        end record;
        type G_Array is array (1 .. 5) of GDec;
    begin
        -- (A) Stand-alone objects:
        declare
            An_Obj : GDec; -- Default initialized.
            Bn_Obj : GDec := 8.00;
        begin
            if An_Obj /= Default then
                Failed ("Bad default initialization of stand alone object " &
                        "of a decimal type - " & Test);
            end if;
            if Bn_Obj /= 8.00 then
                Failed ("Bad explicit initialization of stand alone object " &
                        "of a decimal type - " & Test);
            end if;
        end;
        --------------------------------------------------
        -- (B) Default initialized components:
        declare
            A_Rec : My_Rec;
            A_Arr : G_Array;
        begin
            if A_Rec.Num /= Default then
                Failed ("Bad default initialization of record component of " &
                        "a decimal type - " & Test);
            end if;
            if A_Arr /= (1 .. 5 => Default) then
                Failed ("Bad default initialization of array component of " &
                        "a decimal type - " & Test);
            end if;
        end;
        --------------------------------------------------
        -- (C) <> initialization in array and record aggregates:
        declare
            A_Rec : My_Rec := (Num => <>, Cnt => 12);
            B_Rec : My_Rec := (Num => 5.20, Cnt => 0);
            A_Arr : G_Array := (1 => 5.20, 2 .. 5 => <>);
        begin
            if A_Rec /= (Num  => Default, Cnt => 12) then
                Failed ("Bad default initialization of aggregate record " &
                        "component of a decimal type - " & Test);
            end if;
            if B_Rec /= (Num  => 5.20, Cnt => 0) then
                Failed ("Bad explicit initialization of aggregate record " &
                        "component of a decimal type - " & Test);
            end if;
            if A_Arr /= (1 => 5.20, 2 .. 5 => Default) then
                Failed ("Bad default initialization of aggregate array " &
                        "component of a decimal type - " & Test);
            end if;
        end;

        --------------------------------------------------
        -- (D) Allocators:
        --------------------------------------------------
        declare
            type G_Access is access GDec;
            G_Ptr : G_Access := new GDec; -- Default initialized.

            type Rec_Access is access My_Rec;
            R_Ptr : Rec_Access := new My_Rec; -- Default initialized.

            type Arr_Access is access G_Array;
            A_Ptr : Arr_Access := new G_Array; -- Default initialized.
        begin
            if G_Ptr.all /= Default then
                Failed ("Bad default initialization of allocator of " &
                        "component of a decimal type - " & Test);
            end if;
            if R_Ptr.Num /= Default then
                Failed ("Bad default initialization of record component of " &
                        "allocator of a decimal type - " & Test);
            end if;
            if A_Ptr.all /= (1 .. 5 => Default) then
                Failed ("Bad default initialization of array component of " &
                        "allocator of a decimal type - " & Test);
            end if;
        end;
    end Check_Dec;


 begin
    Test ("C350B02", "Check that objects of types with a specified " &
                     "Default_Value are appropriately initialized. " &
                     "Part 4 - Generic real types");

    declare
        procedure T1 is new Check_Flt (F350B00.Small_Float,
            Default => Ident_SFloat(Flt_Value),
            Test => "Small_Float");

        procedure T2 is new Check_Fix (F350B00.Small_Fixed,
            Default => Ident_SFix(Fix_Value),
            Other_Val => Ident_SFix(1.5),
            Test => "Small_Fix");

        procedure T3 is new Check_Dec (F350B00.Small_Dec,
            Default => Ident_SDec(Dec_Value),
            Test => "Small_Dec");

    begin
        T1;
        T2;
        T3;
    end;

    Result;

end C350B02;
