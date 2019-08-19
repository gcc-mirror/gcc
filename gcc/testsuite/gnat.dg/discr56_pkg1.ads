package Discr56_Pkg1 is

   type Buffer (Size : Positive) is limited private;

private

   type Arr is array (Natural range <>) of Integer;

   protected type Buffer (Size : Positive) is
   private
     Store : Arr (0..Size);
   end Buffer;

end Discr56_Pkg1;
