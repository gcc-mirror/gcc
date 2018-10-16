! { dg-do run }
!
! Ensure that the lower bound starts with the correct
! value
!
! PR fortran/87580
! PR fortran/67125
!
! Contributed by Antony Lewis and mrestelli
!
program p
 implicit none
 integer, allocatable :: a(:), b(:), c(:), d(:), e(:)
 integer :: vec(6)

 vec = [1,2,3,4,5,6]

 allocate(a, source=f(3))
 allocate(b, source=g(3))
 allocate(c, source=h(3))
 allocate(d, source=[1,2,3,4,5])
 allocate(e, source=vec)

 !write(*,*) lbound(a,1), ubound(a,1) ! prints 1 3
 !write(*,*) lbound(b,1), ubound(b,1) ! prints 1 3
 !write(*,*) lbound(c,1), ubound(c,1) ! prints 3 5
 !write(*,*) lbound(d,1), ubound(d,1) ! prints 1 5
 !write(*,*) lbound(e,1), ubound(e,1) ! prints 1 6

 if (lbound(a,1) /= 1 .or. ubound(a,1) /= 3 &
     .or. lbound(b,1) /= 1 .or. ubound(b,1) /= 3 &
     .or. lbound(c,1) /= 3 .or. ubound(c,1) /= 5 &
     .or. lbound(d,1) /= 1 .or. ubound(d,1) /= 5 &
     .or. lbound(e,1) /= 1 .or. ubound(e,1) /= 6) then
   call abort()
 endif
   
contains

 pure function f(i)
  integer, intent(in) :: i
  integer :: f(i)
   f = 2*i
 end function f

 pure function g(i) result(r)
  integer, value, intent(in) :: i
  integer, allocatable :: r(:)
  r = [1,2,3]
 end function g

 pure function h(i) result(r)
  integer, value, intent(in) :: i
  integer, allocatable :: r(:)
  allocate(r(3:5))
  r = [1,2,3]
 end function h
end program p
