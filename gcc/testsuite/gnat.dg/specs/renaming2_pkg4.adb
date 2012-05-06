package body Renaming2_Pkg4 is

  package body Inner is

      function Next_Value return Value_T is
        Next_Value : Value_T renames Value (Next);
      begin
        return Next_Value;
      end Next_Value;

  end Inner;
end Renaming2_Pkg4;
