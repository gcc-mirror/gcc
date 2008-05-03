generic

  type T(<>) is private;

package Discr6_Pkg is

  function X (A : T) return Integer;

  pragma Interface(C, X);
  pragma IMPORT_FUNCTION (
         INTERNAL         => X,
         EXTERNAL         => X,
         PARAMETER_TYPES  => (T),
         MECHANISM        => (Descriptor(S)));

end Discr6_Pkg;
