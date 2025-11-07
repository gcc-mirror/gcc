generic
   type Data_Type (<>) is private;
package Class_Wide6_Pkg is

   type Base_Node_Parser is abstract tagged limited null record;

   function Parse (Parser: Base_Node_Parser) return Data_Type is abstract;

end Class_Wide6_Pkg;
