
package BIP_Prim_Func is
   pragma Elaborate_Body;
        
   type TT is abstract tagged limited record
      I : Integer;
   end record;
        
   function Prim_Func return TT is abstract;
        
end BIP_Prim_Func;
