-- { dg-do compile }
-- { dg-options "-gnatws" }

package body Slice8_Pkg3 is

   Current : Str.Lines (Str.Line_Count);
   Last    : Natural := 0;

   function Get return Str.Paragraph is
      Result : constant Str.Paragraph := (Size => Last,
                                          Data => Current (1..Last));
   begin
      Last := 0;
      return Result;
   end Get;

end Slice8_Pkg3;
