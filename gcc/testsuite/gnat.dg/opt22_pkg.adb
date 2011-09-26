package body Opt22_Pkg is

   procedure Fail is
   begin
      raise Constraint_Error;
   end;

   procedure Put (S : String) is
   begin
      if S /= "the message" then
         raise Program_Error;
      end if;
   end;

end Opt22_Pkg;
