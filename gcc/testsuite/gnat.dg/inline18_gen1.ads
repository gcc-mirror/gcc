generic

  type Bound_T is private;

package Inline18_Gen1 is

  type T is private;
  function Complete return T with Inline_Always;

private

  type T is array (0 .. 1) of Bound_T;

end Inline18_Gen1;
