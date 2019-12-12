generic
  type T is private;
package Generic_Inst12_Pkg1 is

  generic
  procedure Inner_G (Val : T);

  procedure Proc (Val : T);
  pragma Inline (Proc);

end Generic_Inst12_Pkg1;
