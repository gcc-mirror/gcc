-- { dg-do compile }

with Constant1_Pkg;

package Constant1 is

  type Timer_Id_T is new Constant1_Pkg.Timer_Id_T with null record;

  type Timer_Op_T (Pending : Boolean := False) is
     record
       case Pending is
         when True =>
           Timer_Id : Timer_Id_T;
         when False =>
           null;
       end case;
     end record;

  Timer : Timer_Op_T
    := (True, Timer_Id_T'(Constant1_Pkg.Null_Timer_Id with null record));

end Constant1;
