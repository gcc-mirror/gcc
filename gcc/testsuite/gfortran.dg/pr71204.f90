! PR fortran/71204
! { dg-do compile }
! { dg-options "-O0" }

module pr71204
  character(10), allocatable :: z(:)
end module

subroutine s1
  use pr71204
  z(2) = z(1)
end

subroutine s2
  use pr71204
  z(2) = z(1)
end
