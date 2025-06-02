-- C360B02.A

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
--    Part 4: Generic arrays of real types.
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
procedure C360B02 is

    use Report, F360B00;

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


    generic
        type GArr is array (F360B00.Index) of F360B00.Small_Float;
        Default : in GArr; -- The default value.
        Test : in String;
    procedure Check_Flt;

    procedure Check_Flt is
        type My_Rec is record
            Arr  : GArr;
            Cnt  : Natural;
        end record;
        type GArr_Array is array (1 .. 5) of GArr;
    begin
        -- (A) Stand-alone objects:
        declare
            An_Obj : GArr; -- Default initialized.
            Bn_Obj : GArr := (F360B00.Index => 3.0);
        begin
            if An_Obj /= Default then
                Failed ("Bad default initialization of stand alone object " &
                        "of an array of a float type - " & Test);
            end if;
            if Bn_Obj /= (F360B00.Index => 3.0) then
                Failed ("Bad explicit initialization of stand alone object " &
                        "of an array of a float type - " & Test);
            end if;
        end;
        --------------------------------------------------
        -- (B) Default initialized components:
        declare
            A_Rec : My_Rec;
            A_Arr : GArr_Array;
        begin
            if A_Rec.Arr /= Default then
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
            A_Rec : My_Rec := (Arr => <>, Cnt => 12);
            B_Rec : My_Rec := (Arr => (F360B00.Index => 2.5), Cnt => 0);
            A_Arr : GArr_Array := (1 => (F360B00.Index => 2.5), 2 .. 5 => <>);
        begin
            if A_Rec /= (Arr  => Default, Cnt => 12) then
                Failed ("Bad default initialization of aggregate record " &
                        "component of an array of a float type - " & Test);
            end if;
            if B_Rec /= (Arr  => (F360B00.Index => 2.5), Cnt => 0) then
                Failed ("Bad explicit initialization of aggregate record " &
                        "component of an array of a float type - " & Test);
            end if;
            if A_Arr /= (1 => (F360B00.Index => 2.5), 2 .. 5 => Default) then
                Failed ("Bad default initialization of aggregate array " &
                        "component of an array of a float type - " & Test);
            end if;
        end;

        --------------------------------------------------
        -- (D) Allocators:
        --------------------------------------------------
        declare
            type G_Access is access GArr;
            G_Ptr : G_Access := new GArr; -- Default initialized.

            type Rec_Access is access My_Rec;
            R_Ptr : Rec_Access := new My_Rec; -- Default initialized.

            type Arr_Access is access GArr_Array;
            A_Ptr : Arr_Access := new GArr_Array; -- Default initialized.
        begin
            if G_Ptr.all /= Default then
                Failed ("Bad default initialization of allocator of " &
                        "component of an array of a float type - " & Test);
            end if;
            if R_Ptr.Arr /= Default then
                Failed ("Bad default initialization of record component of " &
                        "allocator of an array of a float type - " & Test);
            end if;
            if A_Ptr.all /= (1 .. 5 => Default) then
                Failed ("Bad default initialization of array component of " &
                        "allocator of an array of a float type - " & Test);
            end if;
        end;
    end Check_Flt;


    generic
        type GArr is array (F360B00.Index) of F360B00.Small_Fixed;
        Default : in GArr; -- The default value.
        Other_Val : in GArr; -- Some other value.
        Test : in String;
    procedure Check_Fix;

    procedure Check_Fix is
        type My_Rec is record
            Arr  : GArr;
            Cnt  : Natural;
        end record;
        type GArr_Array is array (1 .. 5) of GArr;
    begin
        -- (A) Stand-alone objects:
        declare
            An_Obj : GArr; -- Default initialized.
            Bn_Obj : GArr := Other_Val;
        begin
            if An_Obj /= Default then
                Failed ("Bad default initialization of stand alone object " &
                        "of an array of a fixed type - " & Test);
            end if;
            if Bn_Obj /= Other_Val then
                Failed ("Bad explicit initialization of stand alone object " &
                        "of an array of a fixed type - " & Test);
            end if;
        end;
        --------------------------------------------------
        -- (B) Default initialized components:
        declare
            A_Rec : My_Rec;
            A_Arr : GArr_Array;
        begin
            if A_Rec.Arr /= Default then
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
            A_Rec : My_Rec := (Arr => <>, Cnt => 12);
            B_Rec : My_Rec := (Arr => Other_Val, Cnt => 0);
            A_Arr : GArr_Array := (1 => Other_Val, 2 .. 5 => <>);
        begin
            if A_Rec /= (Arr  => Default, Cnt => 12) then
                Failed ("Bad default initialization of aggregate record " &
                        "component of an array of a fixed type - " & Test);
            end if;
            if B_Rec /= (Arr  => Other_Val, Cnt => 0) then
                Failed ("Bad explicit initialization of aggregate record " &
                        "component of an array of a fixed type - " & Test);
            end if;
            if A_Arr /= (1 => Other_Val, 2 .. 5 => Default) then
                Failed ("Bad default initialization of aggregate array " &
                        "component of an array of a fixed type - " & Test);
            end if;
        end;

        --------------------------------------------------
        -- (D) Allocators:
        --------------------------------------------------
        declare
            type G_Access is access GArr;
            G_Ptr : G_Access := new GArr; -- Default initialized.

            type Rec_Access is access My_Rec;
            R_Ptr : Rec_Access := new My_Rec; -- Default initialized.

            type Arr_Access is access GArr_Array;
            A_Ptr : Arr_Access := new GArr_Array; -- Default initialized.
        begin
            if G_Ptr.all /= Default then
                Failed ("Bad default initialization of allocator of " &
                        "component of an array of a fixed type - " & Test);
            end if;
            if R_Ptr.Arr /= Default then
                Failed ("Bad default initialization of record component of " &
                        "allocator of an array of a fixed type - " & Test);
            end if;
            if A_Ptr.all /= (1 .. 5 => Default) then
                Failed ("Bad default initialization of array component of " &
                        "allocator of an array of a fixed type - " & Test);
            end if;
        end;
    end Check_Fix;


    generic
        type GArr is array (F360B00.Index) of F360B00.Small_Dec;
        Default : in GArr; -- The default value.
        Test : in String;
    procedure Check_Dec;

    procedure Check_Dec is
        type My_Rec is record
            Arr  : GArr;
            Cnt  : Natural;
        end record;
        type GArr_Array is array (1 .. 5) of GArr;
    begin
        -- (A) Stand-alone objects:
        declare
            An_Obj : GArr; -- Default initialized.
            Bn_Obj : GArr := (F360B00.Index => 8.00);
        begin
            if An_Obj /= Default then
                Failed ("Bad default initialization of stand alone object " &
                        "of an array of a decimal type - " & Test);
            end if;
            if Bn_Obj /= (F360B00.Index => 8.00) then
                Failed ("Bad explicit initialization of stand alone object " &
                        "of an array of a decimal type - " & Test);
            end if;
        end;
        --------------------------------------------------
        -- (B) Default initialized components:
        declare
            A_Rec : My_Rec;
            A_Arr : GArr_Array;
        begin
            if A_Rec.Arr /= Default then
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
            A_Rec : My_Rec := (Arr => <>, Cnt => 12);
            B_Rec : My_Rec := (Arr => (F360B00.Index => 5.20), Cnt => 0);
            A_Arr : GArr_Array := (1 => (F360B00.Index => 5.20), 2 .. 5 => <>);
        begin
            if A_Rec /= (Arr  => Default, Cnt => 12) then
                Failed ("Bad default initialization of aggregate record " &
                        "component of an array of a decimal type - " & Test);
            end if;
            if B_Rec /= (Arr  => (F360B00.Index => 5.20), Cnt => 0) then
                Failed ("Bad explicit initialization of aggregate record " &
                        "component of an array of a decimal type - " & Test);
            end if;
            if A_Arr /= (1 => (F360B00.Index => 5.20), 2 .. 5 => Default) then
                Failed ("Bad default initialization of aggregate array " &
                        "component of an array of a decimal type - " & Test);
            end if;
        end;

        --------------------------------------------------
        -- (D) Allocators:
        --------------------------------------------------
        declare
            type G_Access is access GArr;
            G_Ptr : G_Access := new GArr; -- Default initialized.

            type Rec_Access is access My_Rec;
            R_Ptr : Rec_Access := new My_Rec; -- Default initialized.

            type Arr_Access is access GArr_Array;
            A_Ptr : Arr_Access := new GArr_Array; -- Default initialized.
        begin
            if G_Ptr.all /= Default then
                Failed ("Bad default initialization of allocator of " &
                        "component of an array of a decimal type - " & Test);
            end if;
            if R_Ptr.Arr /= Default then
                Failed ("Bad default initialization of record component of " &
                        "allocator of an array of a decimal type - " & Test);
            end if;
            if A_Ptr.all /= (1 .. 5 => Default) then
                Failed ("Bad default initialization of array component of " &
                        "allocator of an array of a decimal type - " & Test);
            end if;
        end;
    end Check_Dec;


 begin
    Test ("C360B02", "Check that objects of types with a specified " &
                     "Default_Component_Value are appropriately " &
                     "initialized. Part 4 - Generic arrays of real types");

    declare
        procedure T1 is new Check_Flt (F360B00.Float_Array,
            Default => Array_Flt(Flt_Value),
            Test => "Float_Array");

        procedure T2 is new Check_Fix (F360B00.Fixed_Array,
            Default => Array_Fix(Fix_Value),
            Other_Val => Array_Fix(1.5),
            Test => "Fixed_Array");

        procedure T3 is new Check_Dec (F360B00.Dec_Array,
            Default => Array_Dec(Dec_Value),
            Test => "Dec_Array");

    begin
        T1;
        T2;
        T3;
    end;

    Result;

end C360B02;
