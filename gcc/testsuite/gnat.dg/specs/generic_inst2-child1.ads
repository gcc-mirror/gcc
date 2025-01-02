generic
package Generic_Inst2.Child1 is

   function Get_Custom return Custom_Type;

private

   type Dummy is null record;

   Placeholder : constant Dummy := (null record);

   -- This type conversion fails (though only when
   -- instantiated in the other package)
   function Get_Custom return Custom_Type is 
      (Custom_Type(Placeholder'Address));

end Generic_Inst2.Child1;
