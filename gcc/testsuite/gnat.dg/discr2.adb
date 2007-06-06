--  { dg-do compile }

with discr1; use discr1;

package body discr2 is
  
  procedure Copy (Dataset : in out C_Type) is
    Last_Char : Positive := 300;
  begin
    while (Last_Char > 40) loop
      Last_Char := Last_Char - 1;
    end loop;
    
    Assign (Dataset.Designator (1 .. Last_Char));
  end;
  
  procedure Dummy is
  begin
    null;
  end Dummy;

end discr2;
