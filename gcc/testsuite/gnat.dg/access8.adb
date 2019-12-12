--  { dg-do run }
--  { dg-options "-gnatws" }

with Access8_Pkg;
procedure Access8 is
   Errors : Natural := 0;
   outer_object_accessibility_check
     : access Access8_Pkg.object;
   outer_discriminant_accessibility_check
     : access Access8_Pkg.discriminant;
   Mistake
     : access Access8_Pkg.discriminant;
   outer_discriminant_copy_discriminant_check
     : access Access8_Pkg.discriminant;
begin
   declare
      obj
        : aliased Access8_Pkg.object := Access8_Pkg.get;
      inner_object
        : access Access8_Pkg.object := obj'Access;
      inner_discriminant
        : access Access8_Pkg.discriminant := obj.d;
   begin
      begin
         outer_object_accessibility_check
           := inner_object;        --  ERROR
      exception
         when others => Errors := Errors + 1;
      end;
      begin
         Mistake
           := inner_object.d;      --  ERROR
      exception
         when others => Errors := Errors + 1;
      end;
      begin
         outer_discriminant_copy_discriminant_check
           := inner_discriminant;  --  ERROR
      exception
        when others => Errors := Errors + 1;
      end;
      if Errors /= 3 then
         raise Program_Error;
      end if;
   end;
end;
