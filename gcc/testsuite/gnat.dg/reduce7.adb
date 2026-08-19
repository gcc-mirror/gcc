-- { dg-do run }
-- { dg-options "-gnat2022 -gnatVa" }

with Ada.Text_IO;

procedure Reduce7 is

   procedure Section_4_2_1_Paragraph_15 is
      package Roman is
         type Roman_Digit is ('I', 'V', 'X', 'L', 'C', 'D', 'M');
         for Roman_Digit use ('I' => 1, 'V' => 5, 'X' => 10, 'L' => 50, 'C' => 100, 'D' => 500, 'M' => 1000);

         subtype Roman_Character is Wide_Wide_Character with
           Static_Predicate => Roman_Character in 'I' | 'V' | 'X' | 'L' | 'C' | 'D' | 'M';

         Max_Roman_Number : constant := 3_999;  -- MMMCMXCIX

         type Roman_Number is range 1 .. Max_Roman_Number
           with String_Literal => To_Roman_Number;

         function To_Roman_Number (S : Wide_Wide_String) return Roman_Number
           with Pre => S'Length > 0 and then
           (for all Char of S => Char in Roman_Character);

         function To_Roman_Number (S : Wide_Wide_String) return Roman_Number is
           (declare
            R : constant array (Integer range <>) of Roman_Number :=
            (for D in S'Range => Roman_Digit'Enum_Rep
             (Roman_Digit'Wide_Wide_Value (''' & S (D) & '''))); -- See 3.5.2 and 13.4
            begin
              [for I in R'Range =>
                (if I < R'Last and then R (I) < R (I + 1) then -1 else 1) * R (I)]'
                  Reduce("+", 0));
      end Roman;

   use type Roman.Roman_Number;
   X : Roman.Roman_Number := "III" * "IV" * "XII"; -- 144 (that is, CXLIV)
   Y : Roman.Roman_Number := 10;
   begin
      Ada.Text_IO.Put_Line ("III * IV * XII is" & X'Image);
   end Section_4_2_1_Paragraph_15;

   procedure Section_4_5_10_Paragraph_36 is
      use Ada.Text_IO;
      type Real is digits 8;
      -- Example of a reduction expression used to compute the value of Pi:

      --  See 3.5.7.
      function Pi (Number_Of_Steps : Natural := 10_000) return Real is
        (1.0 / Real (Number_Of_Steps) *
          [for I in 1 .. Number_Of_Steps =>
            (4.0 / (1.0 + ((Real (I) - 0.5) * (1.0 / Real (Number_Of_Steps)))**2))]'
              Reduce("+", 0.0));
   begin
      Ada.Text_IO.Put_Line ("Pi =" & Pi (100)'Image);
   end Section_4_5_10_Paragraph_36;

begin
   Section_4_2_1_Paragraph_15;
   Section_4_5_10_Paragraph_36;
end;
