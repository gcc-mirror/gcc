-- { dg-do run }
-- { dg-options "-gnatW8" }

procedure wide_test is
   X  : constant Wide_Character := 'Я';

begin
   declare
      S3 : constant Wide_String := (''', X, ''');
      X3 :           Wide_Character;
   begin
      X3 := Wide_Character'Wide_Value (S3);

      if X /= X3 then
         raise Program_Error;
      end if;
   end;
end;
