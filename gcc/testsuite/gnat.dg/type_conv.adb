-- { dg-do compile }

procedure type_conv is
   type Str is new String;
   generic
   package G is private end;
   package body G is
      Name : constant String := "it";
      Full_Name : Str := Str (Name & " works");
   end G;
   package Inst is new G;
begin
   null;
end;
