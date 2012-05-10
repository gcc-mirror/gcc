with Ada.Streams; use Ada.Streams;

package Lto11 is

   type Vector is array (Positive range <>) of Float;

   procedure Write (S : not null access Root_Stream_Type'Class; V : Vector);

end Lto11;
