--  { dg-do compile }
with Subp_Inst_Pkg;
procedure Subp_Inst is
   procedure Test_Access_Image is
      package Nested is
         type T is private;

         type T_General_Access is access all T;
         type T_Access is access T;
         function Image1 is new Subp_Inst_Pkg.Image (T, T_Access);
         function Image2 is new Subp_Inst_Pkg.Image (T, T_General_Access);
         function Image3 is new Subp_Inst_Pkg.T_Image (T);
      private
         type T is null record;
      end Nested;

      A : aliased Nested.T;
      AG : aliased constant Nested.T_General_Access := A'Access;
      AA : aliased constant Nested.T_Access := new Nested.T;
   begin
      null;
   end Test_Access_Image;

begin
   Test_Access_Image;
end Subp_Inst;
