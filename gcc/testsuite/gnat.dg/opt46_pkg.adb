package body Opt46_Pkg is

   function Last (T : Instance) return Table_Index_Type is
   begin
      return Table_Index_Type (T.P.Last_Val);
   end Last;

end Opt46_Pkg;
