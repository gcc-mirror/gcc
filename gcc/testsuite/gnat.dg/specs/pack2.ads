-- { dg-do compile }

package Pack2 is 
   type Rec is record
       Ptr: access Character;
       Int :Integer;
   end record;
   type Table is array (1..2) of rec;
   pragma Pack (Table);
end Pack2;
