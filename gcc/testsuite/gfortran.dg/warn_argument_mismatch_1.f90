! { dg-do compile }
! { dg-options "-std=legacy -Wno-argument-mismatch" }
!
! No warnings should be output here with -Wno-argument-mismatch.
!

subroutine s1(x)
  implicit none
  integer, intent(in) :: x
  print *, x
end subroutine

subroutine s2(x)
  implicit none
  integer, intent(in) :: x(1)
  print *, x
end subroutine

subroutine s3(x)
  implicit none
  integer, intent(in) :: x(2)
  print *, x
end subroutine

implicit none
integer :: x, y(1)
real :: r

call s1(r)
call s1(y)
call s2(x)
call s3(y)

end
