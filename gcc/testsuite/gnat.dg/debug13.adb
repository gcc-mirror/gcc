-- { dg-do compile }
-- { dg-options "-O2 -g" }

package body Debug13 is

   procedure Compile (P : Natural)
   is
      Max_Pos : constant Natural := P;
      type Position_Set is array (1 .. Max_Pos) of Boolean;

      Empty  : constant Position_Set := (others => False);

      type Position_Set_Array is array (1 .. Max_Pos) of Position_Set;

      Follow  : Position_Set_Array := (others => Empty);

      function Get_Follows return Position_Set;

      procedure Make_DFA;

      function Get_Follows return Position_Set is
         Result : Position_Set := Empty;
      begin
         Result := Result or Follow (1);

         return Result;
      end Get_Follows;

      procedure Make_DFA is
         Next   : constant Position_Set := Get_Follows;
      begin
         null;
      end Make_DFA;

   begin
      Make_DFA;
   end Compile;

end Debug13;
