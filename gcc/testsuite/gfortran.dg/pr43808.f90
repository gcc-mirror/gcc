! PR target/43808
! { dg-do run }
! { dg-options "-O0 -fipa-reference -fschedule-insns -fstrict-aliasing" }

  type :: a
    integer, allocatable :: i(:)
  end type a
  type :: b
    type (a), allocatable :: j(:)
  end type b
  type(a) :: x(2)
  type(b) :: y(2)
  x(1) = a((/1,2,3,4/))
  x(2) = a((/1,2,3,4/)+10)
  y(1) = b((/x(1),x(2)/))
  y(2) = b((/x(1),x(2)/))
  if (y(1)%j(1)%i(1) .ne. 1) STOP 1
end
