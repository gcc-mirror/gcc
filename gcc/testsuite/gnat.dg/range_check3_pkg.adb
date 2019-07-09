package body Range_Check3_Pkg is
   function One return Positive is
   begin
      return 1;
   end One;

   function Zero return Natural is
   begin
      return 0;
   end Zero;

   function Allocate return Array_Access is
   begin
      return
        new Array_Type
             (Positive (One) .. Positive (Zero)); -- Failed range check
   end Allocate;
end Range_Check3_Pkg;
