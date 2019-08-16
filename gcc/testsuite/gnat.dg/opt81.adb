-- { dg-do compile }
-- { dg-options "-O -gnatws" }

with Unchecked_Conversion;

package body Opt81 is

  procedure Copy (From, To : Rec) is
    Len : constant Natural := From.A.all'Length;
    subtype Fixed_String is String (1 .. Len);
    type Fixed_String_Access is access Fixed_String;
    function To_Fixed is new
      Unchecked_Conversion (Source => String_Access,
                            Target => Fixed_String_Access);
    S : Fixed_String_Access := To_Fixed (To.A);
  begin
    S (1 .. Len) := From.A.all;
  end;

end Opt81;
