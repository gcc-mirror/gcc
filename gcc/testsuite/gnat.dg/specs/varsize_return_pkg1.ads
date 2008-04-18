-- { dg-excess-errors "no code generated" }

with Varsize_Return_Pkg2;

generic
  type Id_T is range <>;
package Varsize_Return_Pkg1 is
  
  type Variable_Data_T (Fixed : Boolean := False) is
    record
      case Fixed is
        when True =>
          Length : Natural;
        when False =>
          null;
      end case;
    end record;
  
  function Is_Fixed return Boolean;

  type Variable_Data_Fixed_T is new Variable_Data_T (Is_Fixed);
  
  package Db is new Varsize_Return_Pkg2 (Id_T => Id_T,
                                         Data_T => Variable_Data_Fixed_T);

end Varsize_Return_Pkg1;
