-- { dg-do compile }

with Noreturn4_Pkg; use Noreturn4_Pkg;

package body Noreturn4 is

  procedure P1 (Msg : String) is
  begin
     P1 (Msg, 0);
  end;
  procedure P1 (Msg : String; Val : Integer) is
  begin
     Fatal_Error (Value (It));
  end;

  procedure Fatal_Error (X : Integer) is
  begin
     raise PRogram_Error;
  end;

end Noreturn4;
