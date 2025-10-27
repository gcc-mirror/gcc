generic

  type Number is private;

  with procedure S_One (N: in out Number) is <>;

package Generic_Inst3_Pkg3 is

  procedure One (N: in out Number) renames S_One;

end Generic_Inst3_Pkg3;
