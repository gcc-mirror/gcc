package body Prot5_Pkg is
   protected body P is
      function Get_Data return Integer is
      begin
         return Data;
      end Get_Data;

      procedure Proc (A : Integer := Get_Data) is
      begin
         Data := A * 2;
      end Proc;
   end P;
end Prot5_Pkg;
