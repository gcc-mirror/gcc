--  { dg-do compile }

package access3 is
   type TF is access function return access procedure (P1 : Integer);
   
   type TAF is access protected function return access procedure (P1 : Integer);
   
   type TAF2 is access
     function return access protected procedure (P1 : Integer);
   
   type TAF3 is access
     protected function return access protected procedure (P1 : Integer);
   
   type TAF_Inf is
      access protected function return
         access function return
         access function return
         access function return
         access function return
         access function return
         access function return
         access function return
         access function return
      Integer;
end access3;
