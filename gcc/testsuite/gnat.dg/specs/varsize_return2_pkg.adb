package body Varsize_Return2_Pkg is

   function Len return Positive is
   begin
      return 4;
   end;

   package body G is

      function Get return Small_T is
      begin
         raise Program_Error;
         return Get;
      end;

   end G;

end Varsize_Return2_Pkg;
