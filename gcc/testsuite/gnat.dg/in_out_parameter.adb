-- { dg-do run }

with Ada.Streams.Stream_IO;

procedure In_Out_Parameter is

   use Ada.Streams;  use Stream_IO;

   File : Stream_IO.File_Type;

   type Bitmap is array (Natural range <>) of Boolean;
   for Bitmap'Component_Size use 1;

   type Message   is record
      B : Bitmap (0 .. 14);
   end record;
   for Message use record
      B  at 0 range 2 .. 16;
   end record;

   TX, RX : Message;

begin

   TX.B  := (others => False);
   Stream_IO.Create (File => File, Mode => Out_File, Name => "data");
   Message'Output (Stream (File), TX);
   Stream_IO.Close (File);
   --
   Stream_IO.Open (File => File, Mode => In_File, Name => "data");
   RX := Message'Input (Stream (File));
   Stream_IO.Close (File);

   if RX /= TX then
      raise Program_Error;
   end if;

end In_Out_Parameter;
