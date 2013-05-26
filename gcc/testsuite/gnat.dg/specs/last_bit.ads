-- { dg-do compile }

package Last_Bit is

   Max_Components : constant := 100;
   type Count_Type is new Natural range 0 .. Max_Components;
   subtype Index_Type is Count_Type range 1 .. Count_Type'Last;
   
   type List_Type is array (Index_Type range <>) of Integer;

   type Record_Type (Count : Count_Type := 0) is record
      List : List_Type (1 .. Count);
   end record;

   Null_Record : Record_Type (Count => 0);

   List_Last_Bit : Integer := Null_Record.List'Last_Bit;

end Last_Bit;
