-- { dg-do run }

with System; use System;
with Ada.Text_IO; use Ada.Text_IO;

procedure Conv1 is
   type Short is mod 2**16;

   type R_L is record
      S : Short;
      C : Character;
   end record;
   for R_L'Bit_Order use Low_Order_First;
   for R_L'Scalar_Storage_Order use Low_Order_First;
   for R_L use record
      S at 0 range 0 .. 15;
      C at 2 range 0 .. 7;
   end record;

   type R_H is new R_L;
   for R_H'Bit_Order use High_Order_First;
   for R_H'Scalar_Storage_Order use High_Order_First;
   for R_H use record
      S at 0 range 0 .. 15;
      C at 2 range 0 .. 7;
   end record;

   procedure Dump (Name : String; S : Short; C : Character) is
   begin
      Put_Line (Name & " = (S =>" & S'Img & ", C => '" & C & "')");
   end Dump;

   X_L : R_L;
   X_H : R_H;
begin
   X_L.S := 12345;
   X_L.C := 'a';
   Dump ("X_L", X_L.S, X_L.C);
   -- { dg-output "X_L = \\(S => 12345, C => 'a'\\).*\n" }

   X_H.S := 23456;
   X_H.C := 'b';
   Dump ("X_H", X_H.S, X_H.C);
   -- { dg-output "X_H = \\(S => 23456, C => 'b'\\).*\n" }

   X_H := R_H (X_L);
   Dump ("X_H", X_H.S, X_H.C);
   -- { dg-output "X_H = \\(S => 12345, C => 'a'\\).*\n" }

end;
