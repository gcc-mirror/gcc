with Ada.Assertions;
package body Casesi is

  function Process (X : Natural) return String is
  begin
     case X is
        when 0 => raise Ada.Assertions.Assertion_Error;
        when 1 => raise Ada.Assertions.Assertion_Error;
        when 2 => return (1 .. 4 => 'T');
        when 3 => return (2 .. 8 => 'T');
        when 4 => return "hello";
        when others => return (1 .. 0 => <>);
     end case;
  end;

  procedure Try (X : Natural) is
  begin
     declare
        Code : String := Process (X);
     begin
        if X < 2 then
           raise Program_Error;
        end if;
     end;
  exception
     when Ada.Assertions.Assertion_Error => null;
  end;
end;
