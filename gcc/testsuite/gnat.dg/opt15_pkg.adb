package body Opt15_Pkg is

  procedure Trace_Non_Inlined is
  begin
    raise Program_Error;
  end;

  procedure Trace_Inlined is
  begin
    Trace_Non_Inlined;
  end;

end Opt15_Pkg;
