with Boolean_Subtype2_Pkg; use Boolean_Subtype2_Pkg;

package Boolean_Subtype2 is

   subtype B is Boolean;

   function Is_String_Type (Id : Entity_Id) return B;

end Boolean_Subtype2;
