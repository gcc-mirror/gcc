-- { dg-do compile }
-- { dg-options "-O2" }

with Opt8_Pkg;

package body Opt8 is

    function Content_Value (Rec : Kappa_Component_Rec)
        return Value_Number is
    begin
        return Opt8_Pkg.Id_To_VN (Rec.Content_VN);
    end;

    function Possible_Values_Count (V: Kappa_Component_Ptr) return Natural is
        Result : Natural := 0;
        List : Kappa_Component_Ptr := V;
    begin
        while List /= null loop
            Result := Result +1;
            List := List.Next;
        end loop;
        return Result;
    end;

    function VN_Complexity (Val : Value_Number; N : Natural)
        return Natural is
        Result : Natural := 0;
      begin
        case Val.Kind is
            when Membership_VN =>
                Result :=  VN_Complexity(Val, N);
            when Selected_Address_VN =>
                Result :=  VN_Complexity(Val, N) + 1;
            when Kappa_VN =>
                Result := Possible_Values_Count(Val.Possible_New_Values)*3;
                if Val.Use_Default then
                    if Result < N then
                      Result := Result +
                          VN_Complexity(Content_Value (Val.old_Value), N);
                    end if;
                end if;
            when others =>
              Result := 0;
          end case;
        return Result;
    end;

end Opt8;
