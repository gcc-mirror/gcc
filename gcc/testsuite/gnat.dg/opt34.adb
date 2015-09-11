-- { dg-do compile }
-- { dg-options "-O -fdump-tree-esra" }

with Opt34_Pkg; use Opt34_Pkg;

procedure Opt34 is

   function Local_Fun (Arg : T_Private) return T_Private is
      Result : T_Private;
   begin

      case Get_Integer (Arg) is
         when 1 =>
            Result := Get_Private (100);
         when 2 =>
            Result := T_Private_Zero;
         when others =>
            null;
      end case;

      return Result;
   end Local_Fun;

begin
   Assert (Get_Integer (Local_Fun (Get_Private (1))) = 100);
end;

-- { dg-final { scan-tree-dump "Created a replacement for result" "esra" } }
