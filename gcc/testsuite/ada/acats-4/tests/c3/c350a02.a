-- C350A02.A

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
--    Part 2: Generic discrete types.
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

with Report, F350A00;
procedure C350A02 is

    use Report, F350A00;

    function Ident_SInt (X : in F350A00.Small_Integer)
        return F350A00.Small_Integer is
        -- Force a dynamic value.
    begin
        return F350A00.Small_Integer(Report.Ident_Int(Integer(X)));
    end Ident_SInt;

    function Ident_Status (X : in F350A00.Status) return F350A00.Status is
        -- Force a dynamic value.
    begin
        return F350A00.Status'Val(Report.Ident_Int(F350A00.Status'Pos(X)));
    end Ident_Status;

    function Ident_Word (X : in F350A00.Word_16)
        return F350A00.Word_16 is
        -- Force a dynamic value.
    begin
        if Equal (Ident_Int(1), 1) then
            return X;
        else
            return 0;
        end if;
    end Ident_Word;

    generic
        type GDisc is (<>);
        Default : in GDisc; -- The default value.
        Other_Val : in GDisc; -- Some other value.
        Test : in String;
    procedure Check_Disc;

    procedure Check_Disc is
        type My_Rec is record
            Num  : GDisc;
            Cnt  : Natural;
        end record;
        type G_Array is array (1 .. 5) of GDisc;
    begin
        -- (A) Stand-alone objects:
        declare
            An_Obj : GDisc; -- Default initialized.
            Bn_Obj : GDisc := Other_Val;
        begin
            if An_Obj /= Default then
                Failed ("Bad default initialization of stand alone object " &
                        "of a discrete type - " & Test);
            end if;
            if Bn_Obj /= Other_Val then
                Failed ("Bad explicit initialization of stand alone object " &
                        "of a discrete type - " & Test);
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
                        "a discrete type - " & Test);
            end if;
            if A_Arr /= (1 .. 5 => Default) then
                Failed ("Bad default initialization of array component of " &
                        "a discrete type - " & Test);
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
                        "component of a discrete type - " & Test);
            end if;
            if B_Rec /= (Num  => Other_Val, Cnt => 0) then
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
            type G_Access is access GDisc;
            G_Ptr : G_Access := new GDisc; -- Default initialized.

            type Rec_Access is access My_Rec;
            R_Ptr : Rec_Access := new My_Rec; -- Default initialized.

            type Arr_Access is access G_Array;
            A_Ptr : Arr_Access := new G_Array; -- Default initialized.
        begin
            if G_Ptr.all /= Default then
                Failed ("Bad default initialization of allocator of " &
                        "component of a discrete type - " & Test);
            end if;
            if R_Ptr.Num /= Default then
                Failed ("Bad default initialization of record component of " &
                        "allocator of a discrete type - " & Test);
            end if;
            if A_Ptr.all /= (1 .. 5 => Default) then
                Failed ("Bad default initialization of array component of " &
                        "allocator of a discrete type - " & Test);
            end if;
        end;
    end Check_Disc;


    generic
        type GInt is range <>;
        Default : in GInt; -- The default value.
        Test : in String;
    procedure Check_Int;

    procedure Check_Int is
        type My_Rec is record
            Num  : GInt;
            Cnt  : Natural;
        end record;
        type G_Array is array (1 .. 5) of GInt;
    begin
        -- (A) Stand-alone objects:
        declare
            An_Obj : GInt; -- Default initialized.
            Bn_Obj : GInt := 80;
        begin
            if An_Obj /= Default then
                Failed ("Bad default initialization of stand alone object " &
                        "of an integer type - " & Test);
            end if;
            if Bn_Obj /= 80 then
                Failed ("Bad explicit initialization of stand alone object " &
                        "of an integer type - " & Test);
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
                        "an integer type - " & Test);
            end if;
            if A_Arr /= (1 .. 5 => Default) then
                Failed ("Bad default initialization of array component of " &
                        "an integer type - " & Test);
            end if;
        end;
        --------------------------------------------------
        -- (C) <> initialization in array and record aggregates:
        declare
            A_Rec : My_Rec := (Num => <>, Cnt => 12);
            B_Rec : My_Rec := (Num => 52, Cnt => 0);
            A_Arr : G_Array := (1 => 52, 2 .. 5 => <>);
        begin
            if A_Rec /= (Num  => Default, Cnt => 12) then
                Failed ("Bad default initialization of aggregate record " &
                        "component of an integer type - " & Test);
            end if;
            if B_Rec /= (Num  => 52, Cnt => 0) then
                Failed ("Bad explicit initialization of aggregate record " &
                        "component of an integer type - " & Test);
            end if;
            if A_Arr /= (1 => 52, 2 .. 5 => Default) then
                Failed ("Bad default initialization of aggregate array " &
                        "component of an integer type - " & Test);
            end if;
        end;

        --------------------------------------------------
        -- (D) Allocators:
        --------------------------------------------------
        declare
            type G_Access is access GInt;
            G_Ptr : G_Access := new GInt; -- Default initialized.

            type Rec_Access is access My_Rec;
            R_Ptr : Rec_Access := new My_Rec; -- Default initialized.

            type Arr_Access is access G_Array;
            A_Ptr : Arr_Access := new G_Array; -- Default initialized.
        begin
            if G_Ptr.all /= Default then
                Failed ("Bad default initialization of allocator of " &
                        "component of an integer type - " & Test);
            end if;
            if R_Ptr.Num /= Default then
                Failed ("Bad default initialization of record component of " &
                        "allocator of an integer type - " & Test);
            end if;
            if A_Ptr.all /= (1 .. 5 => Default) then
                Failed ("Bad default initialization of array component of " &
                        "allocator of an integer type - " & Test);
            end if;
        end;
    end Check_Int;


    generic
        type GMod is mod <>;
        Default : in GMod; -- The default value.
        Test : in String;
    procedure Check_Mod;

    procedure Check_Mod is
        type My_Rec is record
            Num  : GMod;
            Cnt  : Natural;
        end record;
        type G_Array is array (1 .. 5) of GMod;
    begin
        -- (A) Stand-alone objects:
        declare
            An_Obj : GMod; -- Default initialized.
            Bn_Obj : GMod := 80;
        begin
            if An_Obj /= Default then
                Failed ("Bad default initialization of stand alone object " &
                        "of a modular type - " & Test);
            end if;
            if Bn_Obj /= 80 then
                Failed ("Bad explicit initialization of stand alone object " &
                        "of a modular type - " & Test);
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
                        "a modular type - " & Test);
            end if;
            if A_Arr /= (1 .. 5 => Default) then
                Failed ("Bad default initialization of array component of " &
                        "a modular type - " & Test);
            end if;
        end;
        --------------------------------------------------
        -- (C) <> initialization in array and record aggregates:
        declare
            A_Rec : My_Rec := (Num => <>, Cnt => 12);
            B_Rec : My_Rec := (Num => 52, Cnt => 0);
            A_Arr : G_Array := (1 => 52, 2 .. 5 => <>);
        begin
            if A_Rec /= (Num  => Default, Cnt => 12) then
                Failed ("Bad default initialization of aggregate record " &
                        "component of a modular type - " & Test);
            end if;
            if B_Rec /= (Num  => 52, Cnt => 0) then
                Failed ("Bad explicit initialization of aggregate record " &
                        "component of a modular type - " & Test);
            end if;
            if A_Arr /= (1 => 52, 2 .. 5 => Default) then
                Failed ("Bad default initialization of aggregate array " &
                        "component of a modular type - " & Test);
            end if;
        end;

        --------------------------------------------------
        -- (D) Allocators:
        --------------------------------------------------
        declare
            type G_Access is access GMod;
            G_Ptr : G_Access := new GMod; -- Default initialized.

            type Rec_Access is access My_Rec;
            R_Ptr : Rec_Access := new My_Rec; -- Default initialized.

            type Arr_Access is access G_Array;
            A_Ptr : Arr_Access := new G_Array; -- Default initialized.
        begin
            if G_Ptr.all /= Default then
                Failed ("Bad default initialization of allocator of " &
                        "component of a modular type - " & Test);
            end if;
            if R_Ptr.Num /= Default then
                Failed ("Bad default initialization of record component of " &
                        "allocator of a modular type - " & Test);
            end if;
            if A_Ptr.all /= (1 .. 5 => Default) then
                Failed ("Bad default initialization of array component of " &
                        "allocator of a modular type - " & Test);
            end if;
        end;
    end Check_Mod;


 begin
    Test ("C350A02", "Check that objects of types with a specified " &
                     "Default_Value are appropriately initialized. " &
                     "Part 2 - Generic discrete types");

    declare
        procedure T1 is new Check_Disc (F350A00.Status,
            Default => Ident_Status(Unknown),
            Other_Val => Ident_Status(Raw),
            Test => "Status");

        procedure T2 is new Check_Disc (F350A00.Small_Integer,
            Default => Ident_SInt(Unused),
            Other_Val => Ident_SInt(12),
            Test => "Short_Integer");

        procedure T3 is new Check_Disc (F350A00.Word_16,
            Default => Ident_Word(16#DEAD#),
            Other_Val => Ident_Word(16#FFFF#),
            Test => "Word_16");

        procedure T4 is new Check_Int (F350A00.Small_Integer,
            Default => Ident_SInt(Unused),
            Test => "Short_Integer");

        procedure T5 is new Check_Mod (F350A00.Word_16,
            Default => Ident_Word(16#DEAD#),
            Test => "Word_16");

    begin
        T1;
        T2;
        T3;
        T4;
        T5;
    end;

    Result;

end C350A02;
