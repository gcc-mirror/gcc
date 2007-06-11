--  { dg-options "-gnatws" }

package body G_Tables is
   function Create (L : Natural) return Table is
   begin
      return T : Table (1 .. L);
   end Create;
end G_Tables;
