generic

  Size : Positive;

package Pack13_Pkg is

  type Object is private;

private

  type Bit is range 0 .. 1;
  for Bit'size use 1;

  type Object is array (1 .. Size) of Bit;
  pragma Pack (Object);

end Pack13_Pkg;
