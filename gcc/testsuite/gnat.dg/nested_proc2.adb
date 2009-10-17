-- { dg-do compile }
-- { dg-options "-gnatws" }

procedure Nested_Proc2 is

   type Arr is array(1..2) of Integer;

   type Rec is record
      Data : Arr;
   end record;

   From  : Rec;
   Index : Integer;

   function F (X : Arr) return Integer is
   begin
      return 0;
   end;

   procedure Test is
   begin
      Index := F (From.Data);
      If Index /= 0 then
         raise Program_Error;
      end if;
   end;

begin
  Test;
end;
