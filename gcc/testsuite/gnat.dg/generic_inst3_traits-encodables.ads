with Ada.Streams;
generic
   pragma Warnings (Off, "is not referenced");
   type T (<>) is private;
   with function Image (Val : in T) return String;
package Generic_Inst3_Traits.Encodables is
   pragma Pure;
end Generic_Inst3_Traits.Encodables;
