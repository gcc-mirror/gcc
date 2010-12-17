-- { dg-do compile }
-- { dg-options "-O" }

with Return2_Pkg; use Return2_Pkg;

package body Return2 is

  function Value_Internal (Image : String) return Result_Internal_T is
  begin
    return (Member => False);
  end;

  type Result_T is array (1 .. 2) of Result_Internal_T;

  function Value (Img : String) return T is
    My_F : constant String := F;
    Result : Result_T;
    Value : T;
  begin
    for I in Result'Range loop
      if G (My_F, I) /= "" then
        Result (I) := Value_Internal (G (My_F, I));
        if Result (I).Member then
          Value (Result (I).Data) := True;
        else
          raise Program_Error;
        end if;
      end if;
    end loop;
    return Value;
  end;

end Return2;
