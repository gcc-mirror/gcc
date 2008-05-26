--  { dg-do compile }

pragma Restrictions (No_Default_Initialization);
package Array_No_Def_Init is
        
   type Int_Array is array (Natural range <>) of Integer;
   IA : Int_Array (1 .. 10);
        
end Array_No_Def_Init;
