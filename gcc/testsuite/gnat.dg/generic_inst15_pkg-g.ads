generic
   Order : Word_Order;
   with procedure Process
     (Word     : in out Word_Type;
      Continue :    out Boolean);
package Generic_Inst15_Pkg.G is
   procedure Translate (Code : in Book_Code_Type) is null;
end Generic_Inst15_Pkg.G;
