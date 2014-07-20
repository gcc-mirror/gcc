package Pack20_Pkg is

   type String_Ptr is access all String;

   procedure Modify (Fixed : in out String_Ptr);

end Pack20_Pkg;
