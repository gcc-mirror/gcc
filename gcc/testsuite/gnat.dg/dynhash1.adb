--  { dg-do run }

with Ada.Text_IO;          use Ada.Text_IO;
with GNAT;                 use GNAT;
with GNAT.Dynamic_HTables; use GNAT.Dynamic_HTables;

procedure Dynhash1 is
   procedure Destroy (Val : in out Integer) is null;
   function Hash (Key : Integer) return Bucket_Range_Type is
   begin
      return Bucket_Range_Type (Key);
   end Hash;

   package Integer_Hash_Tables is new Dynamic_Hash_Tables
     (Key_Type              => Integer,
      Value_Type            => Integer,
      No_Value              => 0,
      Expansion_Threshold   => 1.3,
      Expansion_Factor      => 2,
      Compression_Threshold => 0.3,
      Compression_Factor    => 2,
      "="                   => "=",
      Destroy_Value         => Destroy,
      Hash                  => Hash);
   use Integer_Hash_Tables;

   Siz : Natural;
   T   : Dynamic_Hash_Table;

begin
   T := Create (8);

   Put (T, 1, 1);
   Put (T, 1, 2);
   Put (T, 1, 3);

   Siz := Size (T);

   if Siz /= 1 then
      Put_Line ("ERROR: Put: wrong size");
      Put_Line ("expected: 1");
      Put_Line ("got     :" & Siz'Img);
   end if;

   Delete (T, 1);
   Delete (T, 1);

   Siz := Size (T);

   if Siz /= 0 then
      Put_Line ("ERROR: Delete: wrong size");
      Put_Line ("expected: 0");
      Put_Line ("got     :" & Siz'Img);
   end if;

   Destroy (T);
end Dynhash1;
