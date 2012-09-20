package Noreturn5 is

   procedure Proc (Arg_Line : Wide_String; Keep_Going : Boolean);
   pragma No_Return (Proc);

end Noreturn5;
