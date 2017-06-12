package body Opt64_PKG is
   
   procedure Encode (X : Integer) is
      result : Hash;
   begin
      case X is
         when 1 => result := "1";
         when 2 => result := "2";
         when 3 => result := "3";
         when others => Result := "?";
      end case;     
      Last_Hash := Result;
   end;
end;
