-- C3A2004.A
--
--                             Grant of Unlimited Rights
--
--     The Ada Conformity Assessment Authority (ACAA) holds unlimited
--     rights in the software and documentation contained herein. Unlimited
--     rights are the same as those granted by the U.S. Government for older
--     parts of the Ada Conformity Assessment Test Suite, and are defined
--     in DFAR 252.227-7013(a)(19). By making this public release, the ACAA
--     intends to confer upon all recipients unlimited rights equal to those
--     held by the ACAA. These rights include rights to use, duplicate,
--     release or disclose the released technical data and computer software
--     in whole or in part, in any manner and for any purpose whatsoever, and
--     to have or permit others to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS. THE ACAA MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--
--                                     Notice
--
--     The ACAA has created and maintains the Ada Conformity Assessment Test
--     Suite for the purpose of conformity assessments conducted in accordance
--     with the International Standard ISO/IEC 18009 - Ada: Conformity
--     assessment of a language processor. This test suite should not be used
--     to make claims of conformance unless used in accordance with
--     ISO/IEC 18009 and any applicable ACAA procedures.
--*
--
-- OBJECTIVE:
--      Check that the type of the prefix of X'Access is used to resolve
--      the expected type (both for X as an object and as a subprogram).
--
-- TEST DESCRIPTION:
--      Ada 2005 revised 3.10.2(2) to allow context to help resolve 'Access
--      attributes.
--
--      The revised 3.10.2(2) says that the expected type of an 'Access or
--      'Unchecked_Access attribute is a single access type whose designated
--      type covers the prefix. This rule change allows a variety of
--      interesting overloading scenarios.
--
--      This test is intended to ensure that expressions allowed by the new
--      rule are resolved and implemented properly. We've simplified the
--      types and subprogram names from a realistic case in order to make the
--      test easier to understand and debug if necessary. Even so, it is
--      relatively common that pairs of subprograms end up the with same
--      name (especially via use-visibility) but different profiles. In such
--      cases, resolution should be able to differentiate them.
--
-- CHANGE HISTORY:
--      31 May 00   RLB     Created initial test from now-legal cases from
--                          existing B-Test.
--      18 Dec 14   RLB     Updates for issuance.
--
--!

package C3A2004A is

   type Int_Ptr is access all Integer;
   type Char_Ptr is access all Character;
   type Float_Ptr is access all Float;

   function Zap (Val : Int_Ptr) return Float;
   function Zap (Val : Float_Ptr) return Float;

   type Access_Proc is access procedure (Flag : Boolean);
   type Access_Func is access function (Val : Integer) return Boolean;

   procedure Zip (Val : Access_Proc);
   procedure Zip (Val : Access_Func);

   function Zep (Val : Int_Ptr) return Float;
   function Zep (Val : Access_Func) return Float;

   function Zyp return Int_Ptr;
   function Zyp return Char_Ptr;

   Some_Value : aliased Integer;

   package Pkg is
      type My_Tagged is tagged record
         TC_Count : Natural := 0;
      end record;

      procedure My_Op (Val : access My_Tagged);
   end Pkg;

   package Pkg2 is
      type New_Tagged is new Pkg.My_Tagged with null record;

      procedure My_Op (Val : access New_Tagged); -- Overrides.
   end Pkg2;

   procedure Set_Flag (Flag : Boolean);
   procedure Set_Flag (Value : Integer);

   TC_Set_Flag_Count : Natural := 0;

end C3A2004A;


     --==================================================================--


with Report;
package body C3A2004A is

   function Zap (Val : Int_Ptr) return Float is
   begin
      Val.all := Val.all + 2;
      return Float(Val.all-2);
   end Zap;

   function Zap (Val : Float_Ptr) return Float is
   begin
      Report.Failed("Wrong Zap (Float) called");
      return Val.all;
   end Zap;

   procedure Zip (Val : Access_Proc) is
   begin
      Val (True); -- OK
   end Zip;

   procedure Zip (Val : Access_Func) is
   begin
      Report.Failed("Wrong Zip (Func) called");
      if Val (10) then
         null;
      end if;
   end Zip;

   function Zep (Val : Int_Ptr) return Float is
   begin
      Val.all := Val.all + 4;
      return Float(Val.all-4);
   end Zep;

   function Zep (Val : Access_Func) return Float is
   begin
      if Val (10) then -- OK
         return 2.0;
      else
         return 0.0;
      end if;
   end Zep;

   Result : aliased Integer := 32;

   function Zyp return Int_Ptr is
   begin
      return Result'access;
   end Zyp;

   function Zyp return Char_Ptr is
   begin
      Report.Failed("Wrong Zyp (Char) called");
      return null;
   end Zyp;

   procedure Set_Flag (Flag : Boolean) is
   begin
      if Flag then
         TC_Set_Flag_Count := TC_Set_Flag_Count + 1;
      else
         TC_Set_Flag_Count := TC_Set_Flag_Count + 5;
      end if;
   end Set_Flag;


   procedure Set_Flag (Value : Integer) is
   begin
      Report.Failed("Wrong Set_Flag (Integer) called");
   end Set_Flag;


   package body Pkg is
      procedure My_Op (Val : access My_Tagged) is
      begin
         Val.TC_Count := Val.TC_Count + 1;
      end My_Op;
   end Pkg;

   package body Pkg2 is
      procedure My_Op (Val : access New_Tagged) is
      begin
         Val.TC_Count := Val.TC_Count + 5;
      end My_Op;
   end Pkg2;

end C3A2004A;

with Report, C3A2004A;
procedure C3A2004 is
    use C3A2004A;
begin

    Report.Test ("C3A2004",
                 "Check that 'Access and 'Unchecked_Access attributes can " &
                 "use the type of their prefix in resolving overloading");

    declare
        Value : aliased Integer := 8;
    begin
        -- Check that overloaded subprograms can be resolved if only a single
        -- one makes sense for the access attribute:
        Some_Value := 4;
        if Zap (Int_Ptr'(Some_Value'access)) /= 4.0 then
            -- Qualified expression.
            Report.Failed ("Wrong value from Zap (1A)");
        elsif Some_Value /= 6 then
            Report.Failed ("Wrong value from Zap (1B)");
        end if;
        Some_Value := 6;
        if Zap (Some_Value'access) /= 6.0 then
            -- Resolves using the type of Some_Value.
            Report.Failed ("Wrong value from Zap (1C)");
        elsif Some_Value /= 8 then
            Report.Failed ("Wrong value from Zap (1D)");
        end if;
        if Zap (Int_Ptr'(Value'Unchecked_Access)) /= 8.0 then
            -- Qualified expression.
            Report.Failed ("Wrong value from Zap (1E)");
        elsif Value /= 10 then
            Report.Failed ("Wrong value from Zap (1F)");
        end if;
        Value := 10;
        if Zap (Value'Unchecked_Access) /= 10.0 then
            -- Resolves using the type of Value.
            Report.Failed ("Wrong value from Zap (1G)");
        elsif Value /= 12 then
            Report.Failed ("Wrong value from Zap (1H)");
        end if;
    end;

    declare
        Value : aliased Integer := 5;
    begin
        -- Check that overloaded subprograms can be resolved if both
        -- subprogram and object access types are used:
        Some_Value := 4;
        if Zep (Int_Ptr'(Some_Value'access)) /= 4.0 then
            -- Qualified expression.
            Report.Failed ("Wrong value from Zep (2A)");
        elsif Some_Value /= 8 then
            Report.Failed ("Wrong value from Zep (2B)");
        end if;
        Some_Value := 8;
        if Zep (Some_Value'access) /= 8.0 then
            -- Resolves using the type of Some_Value.
            Report.Failed ("Wrong value from Zep (2C)");
        elsif Some_Value /= 12 then
            Report.Failed ("Wrong value from Zep (2D)");
        end if;
        if Zep (Int_Ptr'(Value'Unchecked_Access)) /= 5.0 then
            -- Qualified expression.
            Report.Failed ("Wrong value from Zep (2E)");
        elsif Value /= 9 then
            Report.Failed ("Wrong value from Zep (2F)");
        end if;
        Value := 9;
        if Zep (Value'Unchecked_Access) /= 9.0 then
            -- Resolves using the type of Value.
            Report.Failed ("Wrong value from Zep (2G)");
        elsif Value /= 13 then
            Report.Failed ("Wrong value from Zep (2H)");
        end if;
    end;

    declare
        Subprogram : Access_Proc := Set_Flag'access;
    begin
        Zip (Access_Proc'(Set_Flag'access));
        if TC_Set_Flag_Count /= 1 then
            Report.Failed ("Set_Flag called wrong number of times (3A)");
            TC_Set_Flag_Count := 1;
        end if;
        Zip (Set_Flag'access);
        if TC_Set_Flag_Count /= 2 then
            Report.Failed ("Set_Flag called wrong number of times (3B)");
            TC_Set_Flag_Count := 2;
        end if;
        Zip (Subprogram);
        if TC_Set_Flag_Count /= 3 then
            Report.Failed ("Set_Flag called wrong number of times (3C)");
            TC_Set_Flag_Count := 3;
        end if;
    end;

    declare
        -- Check that overloading caused by use clauses is handled the
        -- same as direct overloading:
        use Pkg, Pkg2;
        My_Obj : aliased Pkg.My_Tagged;
        New_Obj : aliased Pkg2.New_Tagged;
    begin
        My_Op (New_Obj'access);
        if New_Obj.TC_Count /= 5 then
            Report.Failed ("Wrong My_Op called (4A)");
            New_Obj.TC_Count := 5;
        end if;
        Pkg2.My_Op (New_Obj'access);
        if New_Obj.TC_Count /= 10 then
            Report.Failed ("Wrong My_Op called (4B)");
            New_Obj.TC_Count := 10;
        end if;
        My_Op (My_Obj'Unchecked_Access);
        if My_Obj.TC_Count /= 1 then
            Report.Failed ("Wrong My_Op called (4C)");
            My_Obj.TC_Count := 1;
        end if;
        Pkg.My_Op (My_Obj'Unchecked_Access);
        if My_Obj.TC_Count /= 2 then
            Report.Failed ("Wrong My_Op called (4D)");
            My_Obj.TC_Count := 2;
        end if;
    end;

    -- Check when both the prefix and the subprogram have overloading:
    begin
        if Zap (Int_Ptr'(Zyp.all'access)) /= 32.0 then
            -- Qualified expression, so type is specified.
            Report.Failed ("Wrong value from Zap/Zyp (5A)");
        end if;
        if Zap (Zyp.all'access) /= 34.0 then
            -- Both Zap and Zyp are overloaded.
            Report.Failed ("Wrong value from Zap/Zyp (5B)");
        end if;
        if Zap (Int_Ptr'(Zyp.all'Unchecked_Access)) /= 36.0 then
            -- Qualified expression, so type is specified.
            Report.Failed ("Wrong value from Zap/Zyp (5A)");
        end if;
        if Zap (Zyp.all'Unchecked_Access) /= 38.0 then
            -- Both Zap and Zyp are overloaded.
            Report.Failed ("Wrong value from Zap/Zyp (5B)");
        end if;
    end;

    -- Check when another parameter of the routine provides the resolution:
    declare
        procedure Proc (X : Access_Proc; Y : Integer) is
        begin
            X (Y = 2);
        end Proc;

        procedure Proc (X : Int_Ptr; Y : Float) is
        begin
            X.all := Integer(Y);
        end Proc;

        function Func return Access_Proc is
        begin
            return Set_Flag'access;
        end Func;

        Value : aliased Integer := 4;
        function Func return Int_Ptr is
        begin
            return Value'Unchecked_access;
        end Func;
    begin
        Proc (Access_Proc'(Func.all'access), 2);
        if TC_Set_Flag_Count /= 4 then
            Report.Failed ("Wrong Proc/Func/Set_Flag called (6A)");
            TC_Set_Flag_Count := 4;
        end if;
        Proc (Func.all'access, 4);
        if TC_Set_Flag_Count /= 9 then
            Report.Failed ("Wrong Proc/Func/Set_Flag called (6B)");
            TC_Set_Flag_Count := 9;
        end if;
        Proc (Int_Ptr'(Func.all'Unchecked_Access), 2.0);
        if Value /= 2 then
            Report.Failed ("Wrong Proc called (6C)");
        end if;
        Proc (Func.all'Unchecked_Access, 6.0);
        if Value /= 6 then
            Report.Failed ("Wrong Proc called (6D)");
        end if;
    end;

    Report.Result;

end C3A2004;
