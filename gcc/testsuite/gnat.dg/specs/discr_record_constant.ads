-- { dg-do compile }

pragma Restrictions (No_Implicit_Heap_Allocations);

package Discr_Record_Constant is

   type T (Big : Boolean := False) is record
      case Big is
         when True =>
            Content : Integer;
         when False =>
            null;
       end case;
    end record;

    D : constant T := (True, 0);

    Var :          T := D;    --  OK, maximum size
    Con : constant T := D;    --  Violation of restriction
    Ter : constant T := Con;  --  Violation of restriction

end Discr_Record_Constant;
