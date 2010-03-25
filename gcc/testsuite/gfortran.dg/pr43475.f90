! PR middle-end/43475
! { dg-do compile }
! { dg-options "-O2" }
subroutine ss(w)
  implicit none
  integer :: w(:)
  integer :: b,c,d
  b = w(8)
  c = 5
  d = 3
  call s1(c)
  call s2(b+c)
  call s3(w(b))
end subroutine ss
