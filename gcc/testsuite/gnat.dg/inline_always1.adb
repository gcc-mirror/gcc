--  { dg-do compile }

with Ada.Text_IO;

procedure Inline_Always1 is

   function S(N : Integer ) return String is
   begin
      return "hello world";
   end S;

   type String_Access is access all String;
   type R  is record
      SA : String_Access;
   end record;

   Data : aliased String := "hello world";
   My_SA : constant String_Access :=  Data'Access;
   function Make_R( S : String ) return R is
      My_R : R;
   begin
      My_R.SA := My_SA;
      return My_R;
   end Make_R;

   function Get_String( My_R : R ) return String
   is
   begin
      return S : String(My_R.SA.all'Range) do
         S := My_R.SA.all;
      end return;
   end Get_String;
   pragma Inline_Always( Get_String);

   My_R : constant R := Make_R( "hello world");
begin
   for I in 1..10000 loop
      declare
         Res : constant String := S( 4 );
      begin
         Ada.Text_IO.Put_Line(Res);
      end;
      declare
         Res : constant String := S( 4 );
      begin
         Ada.Text_IO.Put_Line(Res);
      end;

      declare
         S : constant String := Get_String( My_R );
      begin
         Ada.Text_IO.Put_Line(S);
         Ada.Text_IO.Put_Line(My_R.SA.all);
      end;
   end loop;

end Inline_Always1;
