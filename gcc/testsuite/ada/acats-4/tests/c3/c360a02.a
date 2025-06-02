-- C360A02.A

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
--    Part 2: Generic arrays of discrete types.
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

with Report, F360A00;
procedure C360A02 is

    use Report, F360A00;

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

    generic
        type GDisc is (<>);
        type GArr is array (F360A00.Index) of GDisc;
        Default : in GArr; -- The default value.
        Other_Val : in GArr; -- Some other value.
        Test : in String;
    procedure Check_Disc;

    procedure Check_Disc is
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
                        "of an array of a discrete type - " & Test);
            end if;
            if Bn_Obj /= Other_Val then
                Failed ("Bad explicit initialization of stand alone object " &
                        "of an array of a discrete type - " & Test);
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
                        "an array of a discrete type - " & Test);
            end if;
            if A_Arr /= (1 .. 5 => Default) then
                Failed ("Bad default initialization of array component of " &
                        "an array of a discrete type - " & Test);
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
                        "component of a discrete type - " & Test);
            end if;
            if B_Rec /= (Arr  => Other_Val, Cnt => 0) then
                Failed ("Bad explicit initialization of aggregate record " &
                        "component of a discrete type - " & Test);
            end if;
            if A_Arr /= (1 => Other_Val, 2 .. 5 => Default) then
                Failed ("Bad default initialization of aggregate array " &
                        "component of a discrete type - " & Test);
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
                        "component of an array of a discrete type - " & Test);
            end if;
            if R_Ptr.Arr /= Default then
                Failed ("Bad default initialization of record component of " &
                        "allocator of an array of a discrete type - " & Test);
            end if;
            if A_Ptr.all /= (1 .. 5 => Default) then
                Failed ("Bad default initialization of array component of " &
                        "allocator of an array of a discrete type - " & Test);
            end if;
        end;
    end Check_Disc;


    generic
        type GArr is array (F360A00.Index) of F360A00.Small_Integer;
        Default : in GArr; -- The default value.
        Test : in String;
    procedure Check_Int;

    procedure Check_Int is
        type My_Rec is record
            Arr  : GArr;
            Cnt  : Natural;
        end record;
        type GArr_Array is array (1 .. 5) of GArr;
    begin
        -- (A) Stand-alone objects:
        declare
            An_Obj : GArr; -- Default initialized.
            Bn_Obj : GArr := (1 .. 5 => 80);
        begin
            if An_Obj /= Default then
                Failed ("Bad default initialization of stand alone object " &
                        "of an array of an integer type - " & Test);
            end if;
            if Bn_Obj /= (1 .. 5 => 80) then
                Failed ("Bad explicit initialization of stand alone object " &
                        "of an array of an integer type - " & Test);
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
                        "an array of an integer type - " & Test);
            end if;
            if A_Arr /= (1 .. 5 => Default) then
                Failed ("Bad default initialization of array component of " &
                        "an array of an integer type - " & Test);
            end if;
        end;
        --------------------------------------------------
        -- (C) <> initialization in array and record aggregates:
        declare
            A_Rec : My_Rec := (Arr => <>, Cnt => 12);
            B_Rec : My_Rec := (Arr => (1 .. 5 => 52), Cnt => 0);
            A_Arr : GArr_Array := (1 => (1 .. 5 => 52), 2 .. 5 => <>);
        begin
            if A_Rec /= (Arr  => Default, Cnt => 12) then
                Failed ("Bad default initialization of aggregate record " &
                        "component of an array of an integer type - " & Test);
            end if;
            if B_Rec /= (Arr  => (1 .. 5 => 52), Cnt => 0) then
                Failed ("Bad explicit initialization of aggregate record " &
                        "component of an array of an integer type - " & Test);
            end if;
            if A_Arr /= (1 => (1 .. 5 => 52), 2 .. 5 => Default) then
                Failed ("Bad default initialization of aggregate array " &
                        "component of an integer type - " & Test);
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
                        "component of an array of an integer type - " & Test);
            end if;
            if R_Ptr.Arr /= Default then
                Failed ("Bad default initialization of record component of " &
                        "allocator of an array of an integer type - " & Test);
            end if;
            if A_Ptr.all /= (1 .. 5 => Default) then
                Failed ("Bad default initialization of array component of " &
                        "allocator of an array of an integer type - " & Test);
            end if;
        end;
    end Check_Int;


    generic
        type GArr is array (F360A00.Index) of F360A00.Word_16;
        Default : in GArr; -- The default value.
        Test : in String;
    procedure Check_Mod;

    procedure Check_Mod is
        type My_Rec is record
            Arr  : GArr;
            Cnt  : Natural;
        end record;
        type GArr_Array is array (1 .. 5) of GArr;
    begin
        -- (A) Stand-alone objects:
        declare
            An_Obj : GArr; -- Default initialized.
            Bn_Obj : GArr := (1 .. 5 => 80);
        begin
            if An_Obj /= Default then
                Failed ("Bad default initialization of stand alone object " &
                        "of an array of a modular type - " & Test);
            end if;
            if Bn_Obj /= (1 .. 5 => 80) then
                Failed ("Bad explicit initialization of stand alone object " &
                        "of an array of a modular type - " & Test);
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
                        "an array of a modular type - " & Test);
            end if;
            if A_Arr /= (1 .. 5 => Default) then
                Failed ("Bad default initialization of array component of " &
                        "an array of a modular type - " & Test);
            end if;
        end;
        --------------------------------------------------
        -- (C) <> initialization in array and record aggregates:
        declare
            A_Rec : My_Rec := (Arr => <>, Cnt => 12);
            B_Rec : My_Rec := (Arr => (1 .. 5 => 52), Cnt => 0);
            A_Arr : GArr_Array := (1 => (1 .. 5 => 52), 2 .. 5 => <>);
        begin
            if A_Rec /= (Arr  => Default, Cnt => 12) then
                Failed ("Bad default initialization of aggregate record " &
                        "component of an array of a modular type - " & Test);
            end if;
            if B_Rec /= (Arr  => (1 .. 5=> 52), Cnt => 0) then
                Failed ("Bad explicit initialization of aggregate record " &
                        "component of an array of a modular type - " & Test);
            end if;
            if A_Arr /= (1 => (1 .. 5 => 52), 2 .. 5 => Default) then
                Failed ("Bad default initialization of aggregate array " &
                        "component of an array of a modular type - " & Test);
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
                        "component of an array of a modular type - " & Test);
            end if;
            if R_Ptr.Arr /= Default then
                Failed ("Bad default initialization of record component of " &
                        "allocator of an array of a modular type - " & Test);
            end if;
            if A_Ptr.all /= (1 .. 5 => Default) then
                Failed ("Bad default initialization of array component of " &
                        "allocator of an array of a modular type - " & Test);
            end if;
        end;
    end Check_Mod;


 begin
    Test ("C360A02", "Check that objects of types with a specified " &
                     "Default_Component_Value are appropriately " &
                     "initialized. Part 2 - Generic arrays of discrete " &
                     "types");

    declare
        procedure T1 is new Check_Disc (F360A00.Status,
            GArr => F360A00.Status_Array,
            Default => Array_Status(Unknown),
            Other_Val => Array_Status(Raw),
            Test => "Status");

        procedure T2 is new Check_Disc (F360A00.Small_Integer,
            GArr => F360A00.Int_Array,
            Default => Array_Int(Unused),
            Other_Val => Array_Int(12),
            Test => "Int_Array");

        procedure T3 is new Check_Disc (F360A00.Word_16,
            GArr => F360A00.Word_Array,
            Default => Array_Word(16#DEAD#),
            Other_Val => Array_Word(16#FFFF#),
            Test => "Word_Array");

        procedure T4 is new Check_Int (F360A00.Int_Array,
            Default => Array_Int(Unused),
            Test => "Int_Array");

        procedure T5 is new Check_Mod (F360A00.Word_Array,
            Default => Array_Word(16#DEAD#),
            Test => "Word_Array");

    begin
        T1;
        T2;
        T3;
        T4;
        T5;
    end;

    Result;

end C360A02;
