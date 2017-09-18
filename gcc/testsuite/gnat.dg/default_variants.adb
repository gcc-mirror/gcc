--  { dg-do compile }

procedure Default_Variants is

   type Variant_Kind is (A, B);

   function Get_Default_Value (Kind : in Variant_Kind) return Natural is (10);

   type Variant_Type (Kind : Variant_Kind := A) is
      record
         Common : Natural := Get_Default_Value (Kind);
         case Kind is
            when A =>
               A_Value : Integer := Integer'First;
            when B =>
               B_Value : Natural := Natural'First;
         end case;
      end record;

   type Containing_Type is tagged
      record
         Variant_Data : Variant_Type :=
               (Kind => B, Common => <>, B_Value => 1);
      end record;

begin
    null;
end Default_Variants;
