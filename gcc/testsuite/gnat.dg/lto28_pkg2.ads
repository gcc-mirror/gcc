with Lto28_Pkg3;

package Lto28_Pkg2 is

   function F return Lto28_Pkg3.Q_Rec;

   generic
      Q_Conf : Lto28_Pkg3.Q_Rec := F;
   package G is end;

end Lto28_Pkg2;
