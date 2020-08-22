--  { dg-do compile }
--  { dg-options "-O2" }

with Ada.Text_IO;
with Interfaces;

procedure Opt84 is

   type Integer_8  is new Interfaces.Integer_8;
   type Integer_16 is new Interfaces.Integer_16;
   type Integer_32 is new Interfaces.Integer_32;

   type Float_32 is new Interfaces.IEEE_Float_32;

   type Natural_4 is range 0 .. 2 ** 4 - 1;
   for Natural_4'Size use 4;

   type Rec_Type is
      record
         Field_A_Int_8    : Integer_8;
         Field_B_Nat_4    : Natural_4;
         Field_C_Nat_4    : Natural_4;
         Field_D_Int_32   : Integer_32;
         Field_E_Int_32   : Integer_32;
         Field_F_Float_32 : Float_32;
         Field_G_Float_32 : Float_32;
         Field_H_Float_32 : Float_32;
         Field_I_Float_32 : Float_32;
         Field_J_Int_16   : Integer_16;
         Field_K_Int_16   : Integer_16;
         Field_L_Int_16   : Integer_16;
         Field_M_Int_16   : Integer_16;
         Field_N_Float_32 : Float_32;
         Field_O_Float_32 : Float_32;
      end record;
   pragma Pack (Rec_Type);
   for Rec_Type'Alignment use 1;

   procedure Print
     (Item : in Rec_Type) is
   begin
      Ada.Text_IO.Put_Line (Item.Field_F_Float_32'Image);
      Ada.Text_IO.Put_Line (Item.Field_G_Float_32'Image);
      Ada.Text_IO.Put_Line (Item.Field_H_Float_32'Image);
      Ada.Text_IO.Put_Line (Item.Field_I_Float_32'Image);
   end Print;

   procedure Test_Foo is
      Source : Rec_Type;
      Dest   : Rec_Type;
   begin
      Source.Field_A_Int_8 := 0;
      Dest.Field_A_Int_8    := 1;
      Dest.Field_B_Nat_4    := Source.Field_B_Nat_4;
      Dest.Field_C_Nat_4    := Source.Field_C_Nat_4;
      Dest.Field_D_Int_32   := Source.Field_D_Int_32;
      Dest.Field_E_Int_32   := Source.Field_E_Int_32;
      Dest.Field_F_Float_32 := Source.Field_F_Float_32;
      Dest.Field_G_Float_32 := Source.Field_G_Float_32;
      Dest.Field_H_Float_32 := Source.Field_H_Float_32;
      Dest.Field_I_Float_32 := Source.Field_I_Float_32;
      Dest.Field_J_Int_16   := Source.Field_J_Int_16;
      Dest.Field_K_Int_16   := Source.Field_K_Int_16;
      Dest.Field_L_Int_16   := Source.Field_L_Int_16;
      Dest.Field_M_Int_16   := Source.Field_M_Int_16;
      Dest.Field_N_Float_32 := Source.Field_N_Float_32;
      Dest.Field_O_Float_32 := Source.Field_O_Float_32;
      Print (Source);
      Print (Dest);
   end Test_Foo;

begin
   Test_Foo;
end;
