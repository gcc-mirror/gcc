-- { dg-do compile }

pragma Restrictions(No_Elaboration_Code);

package Elab3 is

   type T_List is array (Positive range <>) of Integer;
   type T_List_Access is access constant T_List;

   type R is record
     A : T_List_Access;
   end record;

   C : constant R := (A => null);

end Elab3;
