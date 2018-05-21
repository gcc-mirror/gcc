--  { dg-do run }
--  { dg-output "1 2 3 4 5 6 7 \| 1- 1 2 3 2- 1 2 3 3- 1 2 3 4- 1 2 3 5- 1 2 3" }

with Ada.Text_IO; use Ada.Text_IO;

procedure Exit1 is
   type Int_Range is record
      First, Last : Integer;
   end record
      with Iterable => (First => First,
                        Next => Next,
                        Previous => Previous,
                        Last => Last,
                        Has_Element => Has_Element,
                        Element => Element);

   function First (IR : Int_Range) return Integer is (IR.First);
   function Last (IR : Int_Range) return Integer is (IR.Last);
   function Next (IR : Int_Range; N : Integer) return Integer is (N + 1);
   function Previous (IR : Int_Range; N : Integer) return Integer is (N - 1);
   function Has_Element (IR : Int_Range; N : Integer) return Boolean is
     (N in IR.First ..IR.Last);
   function Element (IR : Int_Range; N : Integer) return Integer is (N);

   IR : Int_Range := (1, 10);

begin
A_Loop:   for I of IR loop
      Put (I'Img);
      exit A_Loop when I = 7;
   end loop A_Loop;
   Put (" | ");

B_Loop:   for I of IR loop
      Put (I'Img & '-');
  C_Loop : for J of IR loop
         Put (J'Img);
         exit when J = 3;
      end loop C_Loop;

      exit B_Loop when I = 5;
   end loop B_Loop;
   New_Line;

end Exit1;
