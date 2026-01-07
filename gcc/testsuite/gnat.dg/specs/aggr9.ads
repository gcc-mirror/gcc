-- PR ada/123289
-- { dg-do compile }
-- { dg-options "-gnat2022" }

package Aggr9 is

   type JSON_Value is tagged null record;
   type JSON_Object is new JSON_Value with null record
     with Aggregate => (Empty => Empty, Add_Named => Insert); -- { dg-error "exactly one" }
   type JSON_Integer is new JSON_Value with null record
     with Integer_Literal => From_Universal_Image;

   function Empty return JSON_Object
   is (null record);

   procedure Insert
      (O : in out JSON_Object; Key : String; Value : JSON_Integer'Class)
   is null;

   procedure Insert (O : in out JSON_Object; Key : String; Value : String)
   is null;

   function From_Universal_Image (Value : String) return JSON_Integer
   is (null record);

end Aggr9;
