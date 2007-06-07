--  { dg-do compile }

package body BIP_Prim_Func is
        
   type NTT is new TT with record
      J : Integer;
   end record;
        
   function Prim_Func return NTT is
   begin
      return Result : NTT := (I => 1, J => 2);
   end Prim_Func;
        
end BIP_Prim_Func;
