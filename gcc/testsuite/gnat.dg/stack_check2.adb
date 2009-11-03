-- { dg-do run }
-- { dg-options "-fstack-check" }

-- This test requires architecture- and OS-specific support code for unwinding
-- through signal frames (typically located in *-unwind.h) to pass.  Feel free
-- to disable it if this code hasn't been implemented yet.

procedure Stack_Check2 is

  function UB return Integer is
  begin
    return 2048;
  end;

  type A is Array (Positive range <>) of Integer;

  procedure Consume_Stack (N : Integer) is
    My_A : A (1..UB); -- 8 KB dynamic
  begin
    My_A (1) := 0;
    if N <= 0 then
      return;
    end if;
    Consume_Stack (N-1);
  end;

  Task T;

  Task body T is
  begin
    begin
      Consume_Stack (Integer'Last);
      raise Program_Error;
    exception
      when Storage_Error => null;
    end;

    Consume_Stack (128);
  end;

begin
  null;
end;
