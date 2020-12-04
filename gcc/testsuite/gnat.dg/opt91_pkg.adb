package body Opt91_Pkg is

   package body Pure_Relation is

      overriding function Custom_Image (Self : Rel) return String is
      begin
         return Custom_Image (Self.Rel);
      end Custom_Image;

   end Pure_Relation;

end Opt91_Pkg;
