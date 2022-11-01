with Lto26_Pkg2; use Lto26_Pkg2;

package body Lto26_Pkg1 is

  procedure Set (R : Rec; A : Stream_Element_Array; C :Unsigned_8) is
    procedure My_Build is new Build;
  begin
     My_Build (A, C);
  end;

end Lto26_Pkg1;
