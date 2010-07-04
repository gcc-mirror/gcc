with Rep_Clause5_Pkg; use Rep_Clause5_Pkg;

package Rep_Clause5 is

    Bad_Number : exception;
    Too_Large  : exception;

    type LNumber_Type is range 0..99999;

    procedure Merge_Numbered(LNodes : in out LNodes_Ptr);

end Rep_Clause5;
