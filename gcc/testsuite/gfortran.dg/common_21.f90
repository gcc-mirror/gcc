! { dg-do compile }
! { dg-options "-std=f2008" }
!
! PR fortran/48858
!
subroutine test
  integer :: l, m
  common /g/ l
  common /jj/ m
  bind(C,name="bar") :: /g/
  bind(C,name="foo") :: /jj/
end

subroutine g
  call jj()
end


