package body Loop_Optimization4_Pkg is

   procedure Add (Phrase : String) is
   begin
      if Debug_Buffer_Len = Max_Debug_Buffer_Len then
         return;
      end if;
      for I in Phrase'Range loop
         Debug_Buffer_Len := Debug_Buffer_Len + 1;
         Debug_Buffer (Debug_Buffer_Len) := Phrase (I);
         if Debug_Buffer_Len = Max_Debug_Buffer_Len then
            exit;
         end if;
      end loop;
   end Add;

end Loop_Optimization4_Pkg;
