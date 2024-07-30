-- { dg-do compile }

procedure Warn13 is

  pragma Warnings ("-Wbogus");  -- { dg-warning "unknown" }
  pragma Warnings ("-Werror");  -- { dg-warning "does not control warning" }
  pragma Warnings ("-Wformat"); -- { dg-warning "switch not valid for Ada" }

begin
  null;
end;
