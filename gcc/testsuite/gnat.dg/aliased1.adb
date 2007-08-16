--  { dg-do compile }
--  { dg-options "-gnatws" }

procedure aliased1 is
  
  type E is (One, Two);
  
  type R (D : E := One) is record
    case D is
      when One =>
         I1 : Integer;
         I2 : Integer;
      when Two =>
         B1 : Boolean;
    end case;
  end record;
  
  type Data_Type is record
    Data : R;
  end record;
  
  type Array_Type is array (Natural range <>) of Data_Type;
  
  function Get return Array_Type is
    Ret : Array_Type (1 .. 2);
  begin
    return Ret;
  end;
  
  Object : aliased Array_Type := Get;

begin
  null;
end;
