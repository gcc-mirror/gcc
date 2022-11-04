with Ada.Streams; use Ada.Streams;
with Interfaces; use Interfaces;

package Lto26_Pkg2 is

  generic
  procedure Build (A : Stream_Element_Array; C : Unsigned_8);

end Lto26_Pkg2;
