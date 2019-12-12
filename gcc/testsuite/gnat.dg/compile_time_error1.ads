with Compile_Time_Error1_Pkg;

package Compile_Time_Error1 is

  type Rec is record
    I : Integer;
  end record;

  package Inst is new Compile_Time_Error1_Pkg (Rec);

  procedure Dummy;

end Compile_Time_Error1;
