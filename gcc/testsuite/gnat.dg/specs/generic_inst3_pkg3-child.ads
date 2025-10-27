generic

  with procedure S_Two (N: in out Number) is <>;

package Generic_Inst3_Pkg3.Child is

  procedure Two (N: in out Number) renames S_Two;

end Generic_Inst3_Pkg3.Child;
