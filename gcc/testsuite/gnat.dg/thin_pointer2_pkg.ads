package Thin_Pointer2_Pkg is

   type SA is access String;
   for SA'Size use Standard'Address_Size;
   S : SA;

   function F return Character;

end Thin_Pointer2_Pkg;
