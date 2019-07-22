--  { dg-do run }

with Ada.Text_IO;    use Ada.Text_IO;
with Encode_String1_Pkg;
with GNAT.Encode_String;
with System.WCh_Con; use System.WCh_Con;

procedure Encode_String1 is
   High_WS  : constant      Wide_String (1000 .. 1009) := (others => '1');
   High_WWS : constant Wide_Wide_String (1000 .. 1009) := (others => '2');
   Low_WS   : constant      Wide_String (3 .. 12) := (others => '3');
   Low_WWS  : constant Wide_Wide_String (3 .. 12) := (others => '4');

   procedure Test_Method (Method : WC_Encoding_Method);
   --  Test Wide_String and Wide_Wide_String encodings using method Method to
   --  encode them.

   -----------------
   -- Test_Method --
   -----------------

   procedure Test_Method (Method : WC_Encoding_Method) is
      package Encoder is new GNAT.Encode_String (Method);

      procedure WS_Tester is new Encode_String1_Pkg
        (C      => Wide_Character,
         S      => Wide_String,
         Encode => Encoder.Encode_Wide_String);

      procedure WWS_Tester is new Encode_String1_Pkg
        (C      => Wide_Wide_Character,
         S      => Wide_Wide_String,
         Encode => Encoder.Encode_Wide_Wide_String);
   begin
      WS_Tester (High_WS);
      WS_Tester (Low_WS);

      WWS_Tester (High_WWS);
      WWS_Tester (Low_WWS);
   end Test_Method;

--  Start of processing for Main

begin
   for Method in WC_Encoding_Method'Range loop
      Test_Method (Method);
   end loop;
end;
