generic

  Size : Positive;

package Pack6_Pkg is

  type Object is private;

private

  type Bit is range 0 .. 1;
  for Bit'Size use 1;

  type Object is array (1 .. Size) of Bit;
  pragma Pack (Object);

end Pack6_Pkg;
