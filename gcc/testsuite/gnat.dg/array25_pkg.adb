package body Array25_Pkg is

   procedure Get_Inner (A : out Arr1) is
   begin
      null;
   end;

   procedure Get (A : out Arr2) is
   begin
      for I in Arr2'Range loop
         Get_Inner (A (I).Data);
      end loop;
   end;

end Array25_Pkg;
