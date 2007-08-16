--  { dg-do run }
--  { dg-options "-gnatws" }

procedure discr4 is
   package Pkg is
      type Rec_Comp (D : access Integer) is record
         Data : Integer;
      end record;
--
      type I is interface;
      procedure Test (Obj : I) is abstract;
--
      Num : aliased Integer := 10;
--
      type Root (D : access Integer) is tagged record
         C1 : Rec_Comp (D);           --  test
      end record;
--
      type DT is new Root and I with null record;
--
      procedure Dummy (Obj : DT);
      procedure Test  (Obj : DT);
   end;
--
   package body Pkg is
      procedure Dummy (Obj : DT) is
      begin
         raise Program_Error;
      end;
--
      procedure Test (Obj : DT) is
      begin
         null;
      end;
   end;
--
   use Pkg;
--
   procedure CW_Test (Obj : I'Class) is
   begin
      Obj.Test;
   end;
--
   Obj : DT (Num'Access);
begin
   CW_Test (Obj);
end;
