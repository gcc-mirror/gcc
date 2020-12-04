package Opt91_Pkg is

   type Base_Relation is abstract tagged null record;

   function Custom_Image (Self : Base_Relation) return String is abstract;

   generic
      type Ty is private;
      with function Custom_Image (Self : Ty) return String is <>;
   package Pure_Relation is

      type Rel is new Base_Relation with record
         Rel : Ty;
      end record;

      overriding function Custom_Image (Self : Rel) return String;
   end Pure_Relation;

end Opt91_Pkg;
