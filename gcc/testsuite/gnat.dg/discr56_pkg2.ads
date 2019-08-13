with Discr56_Pkg1;

package Discr56_Pkg2 is

   type Buffer (Size : Positive) is limited private;

private

   type Buffer (Size : Positive) is new Discr56_Pkg1.Buffer (Size);

end Discr56_Pkg2;
