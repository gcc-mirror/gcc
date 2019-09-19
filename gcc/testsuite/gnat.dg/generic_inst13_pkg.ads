generic
  type Component_T is private;
package Generic_Inst13_Pkg is

  type T is private;

private

  type T is array (Boolean) of Component_T;

end Generic_Inst13_Pkg;
