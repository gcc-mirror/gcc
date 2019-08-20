generic
  type T is private;
package Elab8_Gen is

  procedure Compare (Arg1, Arg2 : T);
  pragma Inline (Compare);

end Elab8_Gen;
