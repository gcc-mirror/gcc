package Loop_Optimization11_Pkg is

   function Img (X : Integer) return String;

   procedure Put_Line (Data : String);

   type Prot is (Execute, Execute_Read, Execute_Read_Write);

   type Mem is (Mem_Image, Mem_Mapped, Mem_Private, Unknown);

end Loop_Optimization11_Pkg;
