package Inline19 is

   protected P is
      function F return String;
      pragma Inline (F);
   end P;

end Inline19;
