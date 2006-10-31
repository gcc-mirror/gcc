-- { dg-do run }
-- { dg-options "-gnata" }

procedure Nested_Return_Test is
   function H (X: integer) return access integer is
      Local : aliased integer := (X+1);
   begin 
      case X is
         when 3 =>
            begin
              return Result : access integer do
                  Result := new integer '(27);
                  begin
                     for I in 1 .. 10 loop
                       result.all := result.all + 10;
                     end loop;
                     return;
                  end;
              end return;
            end;
         when 5 =>
            return Result: Access integer do
               Result := New Integer'(X*X*X);
            end return;
         when others =>
            return null;
      end case;
   end;
begin
   pragma Assert (H (3).all = 127);
   pragma Assert (H (5).all = 125);
   null;
end Nested_Return_Test;
