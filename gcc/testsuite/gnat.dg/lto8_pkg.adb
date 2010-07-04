-- { dg-options "-gnatws" }

package body Lto8_Pkg is

   protected body Protected_Queue_T is
      entry Seize when True is begin null; end;
   end Protected_Queue_T;

end Lto8_Pkg;
