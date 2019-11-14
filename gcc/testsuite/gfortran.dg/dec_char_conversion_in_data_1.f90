! { dg-do run }
! { dg-options "-fdec" }
!
! Modified by Mark Eggleston <mark.eggleston@codethink.com>
!

subroutine normal
  integer(4) :: a
  real(4) :: b
  complex(4) :: c
  logical(4) :: d
  integer(4) :: e
  real(4) :: f
  complex(4) :: g
  logical(4) :: h

  data a / '1234' /
  data b / '1234' /
  data c / '12341234' / ! double the length for complex
  data d / '1234' / 
  data e / 4h1234 /
  data f / 4h1234 /
  data g / 8h12341234 / ! double the length for complex
  data h / 4h1234 /
  
  if (a.ne.e) stop 1
  if (b.ne.f) stop 2
  if (c.ne.g) stop 3
  if (d.neqv.h) stop 4
end subroutine

subroutine padded
  integer(4) :: a
  real(4) :: b
  complex(4) :: c
  logical(4) :: d
  integer(4) :: e
  real(4) :: f
  complex(4) :: g
  logical(4) :: h

  data a / '12' /
  data b / '12' /
  data c / '12334' /
  data d / '123' /
  data e / 2h12 /
  data f / 2h12 /
  data g / 5h12334 /
  data h / 3h123 /
  
  if (a.ne.e) stop 5
  if (b.ne.f) stop 6
  if (c.ne.g) stop 7
  if (d.neqv.h) stop 8
end subroutine

subroutine truncated
  integer(4) :: a
  real(4) :: b
  complex(4) :: c
  logical(4) :: d
  integer(4) :: e
  real(4) :: f
  complex(4) :: g
  logical(4) :: h

  data a / '123478' /
  data b / '123478' /
  data c / '1234123498' /
  data d / '12345' /
  data e / 6h123478 /
  data f / 6h123478 /
  data g / 10h1234123498 /
  data h / 5h12345 /
  
  if (a.ne.e) stop 9
  if (b.ne.f) stop 10
  if (c.ne.g) stop 11
  if (d.neqv.h) stop 12
end subroutine

program test
  call normal
  call padded
  call truncated
end program

