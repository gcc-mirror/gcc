-- { dg-do compile }

with Ada.Exceptions;

package body Noreturn3 is

  procedure Raise_Error (E : Enum; ErrorMessage : String) is

    function Msg return String is
    begin
      return "Error :" & ErrorMessage;
    end;

  begin
    case E is
       when One =>
         Ada.Exceptions.Raise_Exception (Exc1'Identity, Msg);

       when Two =>
         Ada.Exceptions.Raise_Exception (Exc2'Identity, Msg);

       when others =>
         Ada.Exceptions.Raise_Exception (Exc3'Identity, Msg);
    end case;
  end;

end Noreturn3;
