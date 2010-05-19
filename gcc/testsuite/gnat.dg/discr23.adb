--  { dg-do compile }

with Discr23_Pkg; use Discr23_Pkg;

package body Discr23 is

  N : constant Text := Get;

  function Try (A : in Text) return Text is
  begin
    return A;
  exception
    when others => return N;
  end;

  procedure Dummy is begin null; end;

end Discr23;
