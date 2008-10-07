package Loop_Optimization4_Pkg is

   Max_Debug_Buffer_Len : Natural := 8 * 1024;
   Debug_Buffer : String (1 .. Max_Debug_Buffer_Len);
   Debug_Buffer_Len : Natural range 0 .. Max_Debug_Buffer_Len;

   procedure Add (Phrase : String);

end Loop_Optimization4_Pkg;
