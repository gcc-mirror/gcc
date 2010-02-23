! { dg-do compile }
! PR42999  bogus error: Parameter 'i' at (1) has not been declared
! or is a variable, which does not reduce to a constant expression
 TYPE DD
  INTEGER :: I
 END TYPE DD
 TYPE(DD) :: X(2)=(/(DD(I),I=1,2)/)
 END

