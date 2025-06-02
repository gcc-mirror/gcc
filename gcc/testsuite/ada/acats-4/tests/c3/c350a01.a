-- C350A01.A

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
--    Part 1: Normal discrete types.
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
--    We also test that default values out of range raise Constraint_Error.
--
-- CHANGE HISTORY:
--    29 Jun 2020 RLB Created test for RRS, donated to ACATS.

with Report, F350A00;
procedure C350A01 is

    use Report, F350A00;

    type My_Rec is record
        Kind : F350A00.Status;
        Num  : F350A00.Small_Integer;
        Cnt  : F350A00.Word_16;
    end record;

    type Bad_Rec is record
        Cnt : F350A00.Small_Natural;
            -- Default initialization out of range
    end record;

    type Status_Array  is array (1 .. 5) of F350A00.Status;
    type Sml_Int_Array is array (1 .. 5) of F350A00.Small_Integer;
    type Word_Array    is array (1 .. 5) of F350A00.Word_16;

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

    -- Note: We can't safely write a Ident_Word that directly uses Ident_Int
    -- as the range of Integer might not include Word_16'Last.

 begin
    Test ("C350A01", "Check that objects of types with a specified " &
                     "Default_Value are appropriately initialized. " &
                     "Part 1 - Normal discrete types");

    --------------------------------------------------
    -- (A) Stand-alone objects:
    declare
        A_Kind : F350A00.Status; -- Default initialized.
        B_Kind : F350A00.Status := Raw;
        An_Int : F350A00.Small_Integer; -- Default initialized.
        Bn_Int : F350A00.Small_Integer := 0;
        Cn_Int : F350A00.Small_Natural := 1;
        A_Word : F350A00.Word_16; -- Default initialized.
        B_Word : F350A00.Word_16 := 16#FFFF#;
    begin
        if A_Kind /= Ident_Status(Unknown) then
            Failed ("Bad default initialization of stand alone object of " &
                    "an enumeration type");
        end if;
        if B_Kind /= Ident_Status(Raw) then
            Failed ("Bad explicit initialization of stand alone object of " &
                    "an enumeration type");
        end if;
        if An_Int /= Ident_SInt(Unused) then
            Failed ("Bad default initialization of stand alone object of " &
                    "an integer type");
        end if;
        if Bn_Int /= Ident_SInt(0) then
            Failed ("Bad explicit initialization of stand alone object of " &
                    "an integer type");
        end if;
        if Cn_Int /= Ident_SInt(1) then
            Failed ("Bad explicit initialization of stand alone object of " &
                    "an integer subtype");
        end if;
        if A_Word /= Ident_Word(16#DEAD#) then
            Failed ("Bad default initialization of stand alone object of " &
                    "a modular type");
        end if;
        if B_Word /= Ident_Word(16#FFFF#) then
            Failed ("Bad explicit initialization of stand alone object of " &
                    "a modular type");
        end if;
    end;

    begin
        declare
            Dn_Int : F350A00.Small_Natural;
                -- Default initialization out of range
        begin
            Failed ("Default initialization of stand alone object of " &
                    "an integer subtype did not raise Constraint_Error");
        end;
    exception
        when Constraint_Error => null; -- Expected exception.
    end;
    --------------------------------------------------
    -- (B) Default initialized components:
    declare
        A_Rec : My_Rec;
        A_Arr : Status_Array;
        B_Arr : Sml_Int_Array;
        C_Arr : Word_Array;
    begin
        if A_Rec.Kind /= Ident_Status(Unknown) then
            Failed ("Bad default initialization of record component of " &
                    "an enumeration type");
        end if;
        if A_Rec.Num /= Ident_SInt(Unused) then
            Failed ("Bad default initialization of record component of " &
                    "an integer type");
        end if;
        if A_Rec.Cnt /= Ident_Word(16#DEAD#) then
            Failed ("Bad default initialization of record component of " &
                    "a modular type");
        end if;
        if A_Arr /= (1 .. 5 => Ident_Status(Unknown)) then
            Failed ("Bad default initialization of array component of " &
                    "an enumeration type");
        end if;
        if B_Arr /= (1 .. 5 => Ident_SInt(Unused)) then
            Failed ("Bad default initialization of array component of " &
                    "an integer type");
        end if;
        if C_Arr /= (1 .. 5 => 16#DEAD#) then
            Failed ("Bad default initialization of array component of " &
                    "a modular type");
        end if;
    end;

    begin
        declare
            B_Rec : Bad_Rec;
                -- Default initialization out of range
        begin
            Failed ("Default initialization of a record component of" &
                    "an integer subtype did not raise Constraint_Error");
        end;
    exception
        when Constraint_Error => null; -- Expected exception.
    end;

    --------------------------------------------------
    -- (C) <> initialization in array and record aggregates:
    declare
        A_Rec : My_Rec := (Kind => <>, Num => 12, Cnt => 0);
        B_Rec : My_Rec := (Kind => Raw, Num => <>, Cnt => 0);
        C_Rec : My_Rec := (Kind => Raw, Num => 12, Cnt => <>);
        A_Arr : Status_Array := (1 => Solved, 2 .. 5 => <>);
        B_Arr : Sml_Int_Array := (1 => 0, 2 => <>, 3 => -4, 4 .. 5 => <>);
        C_Arr : Word_Array := (1|3|5 => <>, 2 => 0, 4 => 52);
    begin
        if A_Rec /= (Kind => Ident_Status(Unknown),
                     Num  => Ident_SInt(12), Cnt => 0) then
            Failed ("Bad default initialization of aggregate record " &
                    "component of an enumeration type");
        end if;
        if B_Rec /= (Kind => Ident_Status(Raw),
                     Num  => Ident_SInt(Unused), Cnt => 0) then
            Failed ("Bad default initialization of aggregate record " &
                    "component of an integer type");
        end if;
        if C_Rec /= (Kind => Ident_Status(Raw),
                     Num  => Ident_SInt(12), Cnt => 16#DEAD#) then
            Failed ("Bad default initialization of aggregate record " &
                    "component of a modular type");
        end if;
        if A_Arr /= (1 => Ident_Status(Solved),
                     2 .. 5 => Ident_Status(Unknown)) then
            Failed ("Bad default initialization of aggregate array " &
                    "component of an enumeration type");
        end if;
        if B_Arr /= (1 => 0, 3 => -4, 2 | 4 .. 5 => Ident_SInt(Unused)) then
            Failed ("Bad default initialization of aggregate array " &
                    "component of an integer type");
        end if;
        if C_Arr /= (2 => 0, 4 => 52, 1 | 3 | 5 => Ident_Word(16#DEAD#)) then
            Failed ("Bad default initialization of aggregate array " &
                    "component of a modular type");
        end if;
    end;

    begin
        declare
            B_Rec : Bad_Rec := (Cnt => <>);
                -- Default initialization out of range
        begin
            Failed ("Default initialization of an aggregate record " &
                    "component of an integer subtype did not raise " &
                    "Constraint_Error");
        end;
    exception
        when Constraint_Error => null; -- Expected exception.
    end;

    --------------------------------------------------
    -- (D) Allocators:
    --------------------------------------------------
    declare
        type Enum_Access is access F350A00.Status;
        E_Ptr : Enum_Access := new F350A00.Status; -- Default initialized.

        type Int_Access is access F350A00.Small_Integer;
        I_Ptr : Int_Access := new F350A00.Small_Integer;-- Default initialized.

        type Mod_Access is access F350A00.Word_16;
        M_Ptr : Mod_Access := new F350A00.Word_16; -- Default initialized.

        type Rec_Access is access My_Rec;
        R_Ptr : Rec_Access := new My_Rec; -- Default initialized.

        type Arr_Access is access Status_Array;
        A_Ptr : Arr_Access := new Status_Array; -- Default initialized.
    begin
        if E_Ptr.all /= Ident_Status(Unknown) then
            Failed ("Bad default initialization of allocator of " &
                    "an enumeration type");
        end if;
        if I_Ptr.all /= Ident_SInt(Unused) then
            Failed ("Bad default initialization of allocator of " &
                    "an integer type");
        end if;
        if M_Ptr.all /= Ident_Word(16#DEAD#) then
            Failed ("Bad default initialization of allocator of " &
                    "a modular type");
        end if;
        if R_Ptr.Kind /= Ident_Status(Unknown) then
            Failed ("Bad default initialization of record component of " &
                    "allocator of an enumeration type");
        end if;
        if R_Ptr.Num /= Ident_SInt(Unused) then
            Failed ("Bad default initialization of record component of " &
                    "allocator of an integer type");
        end if;
        if R_Ptr.Cnt /= Ident_Word(16#DEAD#) then
            Failed ("Bad default initialization of record component of " &
                    "allocator of a modular type");
        end if;
        if A_Ptr.all /= (1 .. 5 => Ident_Status(Unknown)) then
            Failed ("Bad default initialization of array component of " &
                    "allocator of an enumeration type");
        end if;
    end;

    Result;

end C350A01;
