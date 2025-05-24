-- C413004.A
--
--                             Grant of Unlimited Rights
--
--     AdaCore holds unlimited rights in the software and documentation
--     contained herein. Unlimited rights are the same as those granted
--     by the U.S. Government for older parts of the Ada Conformity
--     Assessment Test Suite, and are defined in DFAR 252.227-7013(a)(19).
--     By making this public release, AdaCore intends to confer upon all
--     recipients unlimited rights equal to those held by the Ada Conformity
--     Assessment Authority. These rights include rights to use, duplicate,
--     release or disclose the released technical data and computer software
--     in whole or in part, in any manner and for any purpose whatsoever,
--     and to have or permit others to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS. ADACORE MAKES NO EXPRESS OR IMPLIED WARRANTY AS
--     TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE SOFTWARE,
--     DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE OR
--     DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
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
--
--     This test is based on one submitted by AdaCore; AdaCore retains
--     the copyright on the test.
--
--*
--  OBJECTIVE:
--
--     Check that for the prefixed view L.R, if L represents an object or value
--     of an access type that designates a tagged type T, that R may represent
--     a subprogram with a first access parameter that designates T that is
--     declared immediately in the declarative region of an ancestor of T.
--
--     Check that for the prefixed view L.R, if L represents an object or value
--     of an access type that designates a tagged type T, that R may represent
--     a subprogram with a first access parameter that designates a
--     classwide type that covers T that is declared immediately
--     in the declarative region of an ancestor of T.
--
--  CHANGE HISTORY:
--     16 Jul 2004  JM   Initial version.
--     27 Sep 2007  RLB  Converted to ACATS format; then created test for
--                       the objective by changing the parameters to
--                       access parameters and the objects to access objects.
--
--!
package C413004P is
   type TP is tagged record
      Data  : Integer := 999;
   end record;

   procedure Base_Proc  (X : access TP);
   procedure Base_Proc  (X : access TP; Value : in Integer);
   function  Base_Func  (X : access constant TP) return Integer;
   function  Base_Func  (X : access constant TP; Value : Integer) return Integer;
   --  These subprograms (Base_Proc and Base_Func) are not redefined
   --  in the extension objects. They are used to check the object.operation
   --  notation applied to inherited subprograms (with and without formals).

   procedure Prim_Proc (X : access TP);
   procedure Prim_Proc (X : access TP; Value : Integer);
   function  Prim_Func (X : access constant TP) return Integer;
   function  Prim_Func (X : access constant TP; Value : Integer) return Integer;
   --  These subprograms (Prim_Proc and Prim_Func) are used to check that
   --  the right subprogram is called when they are redefined in extensions

   procedure Class_Wide_Proc (X : access TP'Class);
   procedure Class_Wide_Proc (X : access TP'Class; Value : Integer);
   function  Class_Wide_Func (X : access constant TP'Class) return Integer;
   function  Class_Wide_Func (X : access constant TP'Class; Value : Integer) return Integer;
   --  These subprograms are used to check that the right class-wide subprogram
   --  is called even when they are redefined in extensions.

end C413004P;

with C413004P;
package C413004Q is
   type TQ is new C413004P.TP with record
      Value : Float := 0.0;
   end record;

   procedure Prim_Proc (X : access TQ);
   procedure Prim_Proc (X : access TQ; Value : Integer);
   function  Prim_Func (X : access constant TQ) return Integer;
   function  Prim_Func (X : access constant TQ; Value : Integer) return Integer;

   procedure Class_Wide_Proc (X : access TQ'Class; Value : Float);
   function  Class_Wide_Func (X : access constant TQ'Class; Value : Float) return Float;
   --  Note: Formals of these class-wide subprograms are different from the
   --        class-wide subprograms defined in the ancestor.

   function Prim_New_Func (X : access constant TQ) return Integer;
   --  This is a new primitive operation.

   package Local is
      type TPP is new TQ with null record;
      procedure Prim_Proc (X : access TPP);
      procedure Prim_Proc (X : access TPP; Value : Integer);
      function  Prim_Func (X : access constant TPP) return Integer;
      function  Prim_Func (X : access constant TPP; Value : Integer) return Integer;
   end Local;
end C413004Q;

package body C413004P is

   procedure Base_Proc (X : access TP) is
   begin
      X.Data := 10;
   end Base_Proc;

   procedure Base_Proc  (X : access TP; Value : Integer) is
   begin
      X.Data := Value;
   end Base_Proc;

   function Base_Func (X : access constant TP) return Integer is
   begin
      return 1;
   end Base_Func;

   function  Base_Func  (X : access constant TP; Value : Integer) return Integer is
   begin
      return Value;
   end Base_Func;

   procedure Prim_Proc (X: access TP) is
   begin
      X.Data := 11;
   end Prim_Proc;

   procedure Prim_Proc (X: access TP; Value : Integer) is
   begin
      X.Data := Value;
   end Prim_Proc;

   function Prim_Func (X : access constant TP) return Integer is
   begin
      return 2;
   end Prim_Func;

   function Prim_Func (X : access constant TP; Value : Integer) return Integer is
   begin
      return Value;
   end Prim_Func;

   procedure Class_Wide_Proc (X: access TP'Class) is
   begin
      X.Data := -1;
   end Class_Wide_Proc;

   procedure Class_Wide_Proc (X: access TP'Class; Value : Integer) is
   begin
      X.Data := 3 * Value;
   end Class_Wide_Proc;

   function Class_Wide_Func (X : access constant TP'Class) return Integer is
   begin
      return -2;
   end Class_Wide_Func;

   function Class_Wide_Func
     (X : access constant TP'Class; Value : Integer) return Integer is
   begin
      return 3 * Value;
   end Class_Wide_Func;

end C413004P;

package body C413004Q is
   procedure Prim_Proc (X: access TQ) is
   begin
      X.Data := 20;
   end Prim_Proc;

   procedure Prim_Proc (X : access TQ; Value : Integer) is
   begin
      X.Data := 2 * Value;
   end Prim_Proc;

   function Prim_Func (X : access constant TQ) return Integer is
   begin
      return 3;
   end Prim_Func;

   function Prim_Func (X : access constant TQ; Value : Integer) return Integer is
   begin
      return 2 * Value;
   end Prim_Func;

   procedure Class_Wide_Proc (X : access TQ'Class; Value : Float) is
   begin
      X.Value := 3.0 * Value;
   end Class_Wide_Proc;

   function Class_Wide_Func (X : access constant TQ'Class; Value : Float) return Float is
   begin
      return 3.0 * Value;
   end Class_Wide_Func;

   function Prim_New_Func (X : access constant TQ) return Integer is
   begin
      return -4;
   end Prim_New_Func;

   package body Local is
      procedure Prim_Proc (X : access TPP) is
      begin
         X.Data := 100;
      end Prim_Proc;

      procedure Prim_Proc (X : access TPP; Value : Integer) is
      begin
         X.Data := 4 * Value;
      end Prim_Proc;

      function Prim_Func (X : access constant TPP) return Integer is
      begin
         return 102;
      end Prim_Func;

      function Prim_Func (X : access constant TPP; Value : Integer) return Integer is
      begin
         return 4 * Value;
      end Prim_Func;
   end Local;
end C413004Q;

with Report;
with C413004Q;
procedure C413004 is
begin
   Report.Test ("C413004", "Check that for the prefixed view L.R, if L " &
                           "represents an object or value of an access type " &
                           "that designates a tagged type T, " &
                           "that R may represent a subprogram with a first " &
                           "access parameter that designates the type T or " &
                           "a class-wide type covered by T that is declared " &
                           "immediately in the declarative region of " &
                           "an ancestor of T.");

   -- Verify that the primitive operation in the ancestor is properly called.

   declare
      Q1_Ptr  : access C413004Q.TQ := new C413004Q.TQ;
      Q2_Ptr  : access C413004Q.TQ := new C413004Q.TQ;
   begin
      -- -------------------------------------------------------------------
      -- call on inherited procedure (with no parameters)

      Q1_Ptr.Base_Proc;
      C413004Q.Base_Proc (Q2_Ptr);
      if Q1_Ptr.Data /= Q2_Ptr.Data then
         Report.Failed ("Wrong value (inherited procedure -1-)");
      end if;

      -- call on inherited function (with no parameters)
      if Q1_Ptr.Base_Func /= C413004Q.Base_Func (Q1_Ptr) then
         Report.Failed ("Wrong value (inherited function -2-)");
      end if;

      -- call on inherited procedure (with parameters)
      Q1_Ptr.Base_Proc (Value => 123);
      if Q1_Ptr.Data /= 123 then
         Report.Failed ("Wrong value (inherited procedure -3-)");
      end if;

      --  call on inherited function (with parameters)
      if Q1_Ptr.Base_Func (Value => 1234)
        /= C413004Q.Base_Func (Q1_Ptr, 1234)
      then
         Report.Failed ("Wrong value (inherited function -4-)");
      end if;
   end;

   -- -----------------------------------------------------------------
   --  Verify that the redefined primitive operation is properly called.

   declare
      Q1_Ptr  : access C413004Q.TQ := new C413004Q.TQ;
      Q2_Ptr  : access C413004Q.TQ := new C413004Q.TQ;
   begin
      --  call on primitive procedure (with no parameters)

      Q1_Ptr.Prim_Proc;
      C413004Q.Prim_Proc (Q2_Ptr);
      if Q1_Ptr.Data /= Q2_Ptr.Data then
         Report.Failed ("Wrong value (primitive procedure -1-)");
      end if;

      --  call on primitive function (with no parameters)
      if Q1_Ptr.Prim_Func /= C413004Q.Prim_Func (Q1_Ptr) then
         Report.Failed ("Wrong value (primitive function -2-)");
      end if;

      --  call on primitive procedure (with parameters)
      Q1_Ptr.Prim_Proc (Value => 123);
      C413004Q.Prim_Proc (Q2_Ptr, Value => 123);
      if Q1_Ptr.Data /= Q2_Ptr.Data then
         Report.Failed ("Wrong value (primitive procedure -3-)");
      end if;

      --  call on primitive function (with parameters)
      if Q1_Ptr.Prim_Func (Value => 123)
        /= C413004Q.Prim_Func (Q1_Ptr, Value => 123)
      then
         Report.Failed ("Wrong value (primitive function -4-)");
      end if;
   end;

   -- -------------------------------------------------------------------
   --  Verify dispatching calls.

   declare
      Q1_Ptr  : access C413004Q.TQ := new C413004Q.TQ;
      Q2_Ptr  : access C413004Q.TQ := new C413004Q.TQ;
      Q1_CPtr : access C413004Q.TQ'Class := Q1_Ptr;
      Q2_CPtr : access C413004Q.TQ'Class := Q2_Ptr;
   begin
      --  dispatching call to procedure (with no parameters)
      Q1_CPtr.Base_Proc;
      C413004Q.Base_Proc (Q2_CPtr);
      if Q1_Ptr.Data /= Q2_Ptr.Data then
         Report.Failed ("Wrong value (dispatching call to procedure -1-)");
      end if;

      --  dispatching call to funcion (with no parameters)
      if Q1_CPtr.Base_Func /= C413004Q.Base_Func (Q1_CPtr) then
         Report.Failed ("Wrong value (dispatching call to function -2-)");
      end if;

      -- call on inherited procedure (with parameters)
      Q1_CPtr.Base_Proc (Value => 123);
      if Q1_CPtr.Data /= 123 then
         Report.Failed ("Wrong value (inherited procedure -3-)");
      end if;

      --  call on inherited function (with parameters)
      if Q1_CPtr.Base_Func (Value => 1234)
        /= C413004Q.Base_Func (Q1_Ptr, 1234)
      then
         Report.Failed ("Wrong value (inherited function -4-)");
      end if;

      --  dispatching call on new primitive function
      if Q1_CPtr.Prim_New_Func /= C413004Q.Prim_New_Func (Q1_CPtr) then
         Report.Failed ("Wrong value (dispatching call on "
                        & "primitive function)");
      end if;
   end;

   -- -------------------------------------------------------------------
   --  Test class-wide subprograms.

   declare
      Q_Ptr  : access C413004Q.TQ := new C413004Q.TQ;
   begin
      --  call class-wide procedure (with no parameters)
      Q_Ptr.Class_Wide_Proc;                  --  Call P.Class_Wide_Proc
      if Q_Ptr.Data /= -1 then
         Report.Failed ("Wrong value (class-wide procedure -1-)");
      end if;

      --  call class-wide function (with no parameters)
      if Q_Ptr.Class_Wide_Func /= -2 then     --  Call P.Class_Wide_Func
         Report.Failed ("Wrong value (class-wide function -2-)");
      end if;

      --  call class-wide procedure (with parameters)
      Q_Ptr.Class_Wide_Proc (Value => 3);     --  Call P.Class_Wide_Proc
      if Q_Ptr.Data /= 9 then
         Report.Failed ("Wrong value (class-wide procedure -3-)");
      end if;

      --  call class-wide function (with parameters)
      if Q_Ptr.Class_Wide_Func (Value => 3) /= 9 then -- P.Call_Wide_Func
         Report.Failed ("Wrong value (class-wide function -4-)");
      end if;

      --  call class-wide procedure (with parameters)
      Q_Ptr.Class_Wide_Proc (Value => 3.0);
      --  C413004Q.Class_Wide_Proc
      if Q_Ptr.Value /= 9.0 then
         Report.Failed ("Wrong value (class-wide procedure -5-)");
      end if;

      --  call class-wide function (with parameters)
      if Q_Ptr.Class_Wide_Func (Value => 3.0) /= 9.0 then
         -- C413004Q.Call_Wide_Func
         Report.Failed ("Wrong value (class-wide function -6-)");
      end if;
   end;

   -- -------------------------------------------------------------------
   --  Test nested package.

   declare
      L1_Ptr : access C413004Q.Local.TPP := new C413004Q.Local.TPP;
      L2_Ptr : access C413004Q.Local.TPP := new C413004Q.Local.TPP;
   begin
      --  call on primitive procedure in nested package (with no parameters)
      L1_Ptr.Prim_Proc;
      C413004Q.Local.Prim_Proc (L2_Ptr);
      if L1_Ptr.Data /= L2_Ptr.Data then
         Report.Failed ("Wrong value (primitive op in nested package -1-)");
      end if;

      --  call on primitive function in nested package (with no parameters)
      if L1_Ptr.Prim_Func /= C413004Q.Local.Prim_Func (L1_Ptr) then
         Report.Failed ("Wrong value (primitive op in nested package -2-)");
      end if;

      --  call on primitive procedure in nested package (with parameters)
      L1_Ptr.Prim_Proc (Value => 123);
      C413004Q.Local.Prim_Proc (L2_Ptr, Value => 123);
      if L1_Ptr.Data /= L2_Ptr.Data then
         Report.Failed ("Wrong value (primitive op in nested package -3-)");
      end if;

      --  call on primitive function in nested package (with parameters)
      if L1_Ptr.Prim_Func (Value => 123)
        /= C413004Q.Local.Prim_Func (L1_Ptr, Value => 123)
      then
         Report.Failed ("Wrong value (primitive function -4-)");
      end if;
   end;

   Report.Result;
end C413004;
