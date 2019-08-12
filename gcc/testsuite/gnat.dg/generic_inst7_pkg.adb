package body Generic_Inst7_Pkg is

   use type Generic_Inst7_Types.Index;

   procedure Process (List : in out Generic_Inst7_Types.List) is
   begin
      for I in Generic_Inst7_Types.Index range 1 .. List.Arr'length loop
         null;
      end loop;
   end Process;

end Generic_Inst7_Pkg;
