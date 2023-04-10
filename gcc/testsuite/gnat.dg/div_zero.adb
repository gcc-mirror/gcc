-- { dg-do run }
-- { dg-skip-if "divide does not trap" { aarch64*-*-* powerpc*-*-* } }

-- This test requires architecture- and OS-specific support code for unwinding
-- through signal frames (typically located in *-unwind.h) to pass.  Feel free
-- to disable it if this code hasn't been implemented yet.

procedure Div_Zero is

  pragma Suppress (All_Checks);

  function Zero return Integer is
  begin
    return 0;
  end;

  D : Integer := Zero;

begin
  D := 1 / D;
  raise Program_Error;
exception
  when Constraint_Error => null;
end;
