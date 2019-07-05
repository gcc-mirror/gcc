--  { dg-do run }
--  { dg-options "-gnat12" }

procedure Access6 is
   type Int_Ref is access all Integer;
   Ptr : Int_Ref;

   procedure update_ptr (X : access integer) is
   begin
      --  Failed accessibility test: supposed to raise a Program_Error
      Ptr := Int_Ref (X);
   end;

   procedure bar is
      ref : access integer := new integer;
   begin
      update_ptr (ref);
   end;
begin
   bar;

   --  As the call to bar must raise a Program_Error, the following is not supposed to be executed:
   raise Constraint_Error;

exception
   when Program_Error =>
      null;
end;
