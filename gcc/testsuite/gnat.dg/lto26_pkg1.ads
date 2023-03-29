with Ada.Finalization;
with Ada.Streams;  use Ada.Streams;
with Interfaces; use Interfaces;

package Lto26_Pkg1 is

  type Rec is new Ada.Finalization.Limited_Controlled with null record;

  procedure Set (R : Rec; A : Stream_Element_Array; C :Unsigned_8);

end Lto26_Pkg1;
