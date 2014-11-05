with Ada.Text_IO; use Ada.Text_IO;

package body Inline11_Pkg is

  function My_Img (I : Integer) return String is
  begin
    return I'Img;
  end;

  procedure Trace (I : Integer) is
  begin
    Put_Line (My_Img (I));
  end;

end Inline11_Pkg;
