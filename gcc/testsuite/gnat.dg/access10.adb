--  PR ada/113893
--  Testcase by Pascal Pignard <p.p11@orange.fr>

--  { dg-do run }

with Ada.Text_IO;
with Ada.Finalization;

procedure Access10 is

   generic
      type Element_Type is private;
      with function Image (Item : Element_Type) return String is <>;
   package Sanitize is
      type Container is new Ada.Finalization.Controlled with record
         Data : Element_Type;
      end record;
      overriding procedure Finalize (Object : in out Container);
   end Sanitize;

   package body Sanitize is
      overriding procedure Finalize (Object : in out Container) is
      begin
         Ada.Text_IO.Put_Line ("Current:" & Image (Object.Data));
      end Finalize;
   end Sanitize;

   procedure Test01 is
      package Float_Sanitized is new Sanitize (Float, Float'Image);
      V  : Float_Sanitized.Container;
      C  : constant Float_Sanitized.Container :=
	     (Ada.Finalization.Controlled with 8.8);
      A  : access Float_Sanitized.Container := 
	     new Float_Sanitized.Container'(Ada.Finalization.Controlled with 7.7);  -- { dg-warning "not be finalized|named" }
      AC : access constant Float_Sanitized.Container :=
             new Float_Sanitized.Container'(Ada.Finalization.Controlled with 6.6);  -- { dg-warning "not be finalized|named" }
   begin
      V.Data := 9.9 + C.Data + A.Data;
      Ada.Text_IO.Put_Line ("Value:" & Float'Image (V.Data));
   end Test01;

   procedure Test02 is
      type Float_Sanitized is new Float;
      V  : Float_Sanitized;
      C  : constant Float_Sanitized        := (8.8);
      A  : access Float_Sanitized          := new Float_Sanitized'(7.7);
      AC : access constant Float_Sanitized := new Float_Sanitized'(6.6);
   begin
      V := 9.9 + C + A.all;
      Ada.Text_IO.Put_Line ("Value:" & Float_Sanitized'Image (V));
   end Test02;

begin
   Ada.Text_IO.Put_Line ("Test01:");
   Test01;
   Ada.Text_IO.Put_Line ("Test02:");
   Test02;
end;
