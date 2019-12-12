
--  gen.ads

with Inline15_Types;

generic
package Inline15_Gen is
   function Func (Val : Inline15_Types.Enum) return Inline15_Types.Rec with Inline;

   procedure Call_Func with Inline;
end Inline15_Gen;
