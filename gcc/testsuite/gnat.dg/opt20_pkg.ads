package Opt20_Pkg is

   procedure Write_Str (S : String);

   type Fail_Proc is access procedure (S : String);

   procedure My_Fail (S : String);

   Fail : Fail_Proc := My_Fail'Access;

   function Get_Name_String (Id : Integer) return String;

end Opt20_Pkg;
