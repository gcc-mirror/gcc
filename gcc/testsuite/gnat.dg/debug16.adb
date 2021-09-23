-- { dg-do compile }
-- { dg-skip-if "No Dwarf" { { hppa*-*-hpux* } && { ! lp64 } } }
-- { dg-options "-cargs -O0 -g -dA -fgnat-encodings=minimal -margs" }

procedure Debug16 is

   type Number_T (Exists : Boolean := False) is
      record
         case Exists is
            when True =>
               Value : Natural range 0 .. 255;
            when False =>
               null;
         end case;
      end record;
   pragma Pack (Number_T);

   X : Number_T;

begin
   X := (Exists => True, Value => 10);
   if X.Exists then -- STOP
      X.Value := X.Value + 1;
   end if;
end;

-- { dg-final { scan-assembler-times "DW_AT_discr" 4 } }
