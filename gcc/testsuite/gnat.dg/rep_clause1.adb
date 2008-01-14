--  { dg-do compile }

with Ada.Text_IO; use Ada.Text_IO;

procedure Rep_Clause1 is
   
   type Int_16 is range 0 .. 65535;
   for Int_16'Size use 16;
   
   ----------------------------------------------
      
   type Rec_A is
      record
         Int_1 : Int_16;
         Int_2 : Int_16;
         Int_3 : Int_16;
         Int_4 : Int_16;
      end record;
      
      
   for Rec_A use record
      Int_1 at 0 range  0 .. 15;
      Int_2 at 2 range  0 .. 15;
      Int_3 at 4 range  0 .. 15;
      Int_4 at 6 range  0 .. 15;
   end record;
   
   Rec_A_Size : constant := 4 * 16;
   
   for Rec_A'Size use Rec_A_Size;
   
   ----------------------------------------------
   
   type Rec_B_Version_1 is
      record
         Rec_1 : Rec_A;
         Rec_2 : Rec_A;
         Int_1 : Int_16;
      end record;
  
   for Rec_B_Version_1 use record
      Rec_1 at  0 range  0 .. 63;
      Rec_2 at  8 range  0 .. 63;
      Int_1 at 16 range  0 .. 15;
   end record;
  
   Rec_B_Size : constant := 2 * Rec_A_Size + 16;
   
   for Rec_B_Version_1'Size use Rec_B_Size;
   for Rec_B_Version_1'Alignment use 2;

   ----------------------------------------------

   type Rec_B_Version_2 is
      record
         Int_1 : Int_16;
         Rec_1 : Rec_A;
         Rec_2 : Rec_A;
      end record;
   
   for Rec_B_Version_2 use record
      Int_1 at  0 range  0 .. 15;
      Rec_1 at  2 range  0 .. 63;
      Rec_2 at 10 range  0 .. 63;
   end record;

   for Rec_B_Version_2'Size use Rec_B_Size;
   
   ----------------------------------------------
   
   Arr_A_Length : constant := 2;
   Arr_A_Size   : constant := Arr_A_Length * Rec_B_Size;
   
   type Arr_A_Version_1 is array (1 .. Arr_A_Length) of Rec_B_Version_1;
   type Arr_A_Version_2 is array (1 .. Arr_A_Length) of Rec_B_Version_2;
   
   pragma Pack (Arr_A_Version_1);
   pragma Pack (Arr_A_Version_2);
   
   for Arr_A_Version_1'Size use Arr_A_Size;
   for Arr_A_Version_2'Size use Arr_A_Size;
   
   ----------------------------------------------

begin
   --  Put_Line ("Arr_A_Size =" & Arr_A_Size'Img);
   
   if Arr_A_Version_1'Size /= Arr_A_Size then
      Ada.Text_IO.Put_Line
        ("Version 1 Size mismatch! " &
         "Arr_A_Version_1'Size =" & Arr_A_Version_1'Size'Img);
   end if;
   
   if Arr_A_Version_2'Size /= Arr_A_Size then
      Ada.Text_IO.Put_Line
        ("Version 2 Size mismatch! " &
         "Arr_A_Version_2'Size =" & Arr_A_Version_2'Size'Img);
   
   end if;

end;
