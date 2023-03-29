package body Lto26_Pkg2 is

  procedure Build (A : Stream_Element_Array; C : Unsigned_8) is
    Start  : Stream_Element_Offset := A'First;
    Next   : Stream_Element_Offset;
    Length : Unsigned_8;
  begin
    for I in 1 .. C loop
      Length := Unsigned_8 (A (A'First));
      Next   := Start + Stream_Element_Offset (Length);
      Start  := Next;
    end loop;
  end;

end Lto26_Pkg2;
