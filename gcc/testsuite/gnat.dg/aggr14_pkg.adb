package body Aggr14_Pkg  is

   function F return A is
   begin
      if X /= (1, 2, 3) then
        raise Program_Error;
      end if;
      return (1, 1, 1);
   end;

   procedure Proc is
   begin
    X := F;
  end;

end Aggr14_Pkg;
