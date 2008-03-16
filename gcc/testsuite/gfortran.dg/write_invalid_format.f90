! { dg-do "compile" }
! PR fortran/35582 - ICE on invalid format
! Testcase contributed by
! Leandro Martinez <leandromartinez DOT spam AT gmail DOT com> 

  real, parameter  :: a = 1.
  write(*,a) 'test'           ! { dg-error "expression in FORMAT tag" }
end

