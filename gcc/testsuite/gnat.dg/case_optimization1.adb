-- { dg-do compile }
-- { dg-options "-O2" }

package body Case_Optimization1 is

   function F (Op_Kind : Internal_Operator_Symbol_Kinds) return Integer is
   begin
      case Op_Kind is
         when A_Not_Operator => return 3;
         when An_Exponentiate_Operator => return 2;
         when others => return 1;
      end case;
   end;

   function Len (E : Element) return Integer is
      Op_Kind : Internal_Element_Kinds := Int_Kind (E);
   begin
      return F (Int_Kind (E));
   end;

end Case_Optimization1;
