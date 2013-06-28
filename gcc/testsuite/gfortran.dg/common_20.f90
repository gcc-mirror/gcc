! { dg-do compile }
! { dg-options "-std=f2003" }
!
! PR fortran/48858
!
subroutine test
  integer :: l, m
  common /g/ l ! { dg-error "Fortran 2008: COMMON block 'g' with binding label at .1. sharing the identifier with global non-COMMON-block entity at .2." }
  common /jj/ m ! { dg-error "Global name 'jj' at .1. is already being used as a COMMON at .2." }
  bind(C,name="bar") :: /g/
  bind(C,name="foo") :: /jj/
end

subroutine g ! { dg-error "Fortran 2008: COMMON block 'g' with binding label at .1. sharing the identifier with global non-COMMON-block entity at .2." }
  call jj()  ! { dg-error "Global name 'jj' at .1. is already being used as a COMMON at .2." }
end


