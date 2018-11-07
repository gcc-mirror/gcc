package Prot5_Pkg is
   protected P is
      function Get_Data return Integer;
      procedure Proc (A : Integer := Get_Data);
   private
      Data : Integer;
   end P;
end Prot5_Pkg;
