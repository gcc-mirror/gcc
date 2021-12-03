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
 type t
   integer :: i
 end type t
 class(t), allocatable :: p1(:), p2(:), p3(:), p4(:)
 integer :: vec(6)

 vec = [1,2,3,4,5,6]

 allocate(a, source=f(3))
 allocate(b, source=g(3))
 allocate(c, source=h(3))
 allocate(d, source=[1,2,3,4,5])
 allocate(e, source=vec)

 allocate(p1(3:4))
 p1(:)%i = [43,56]
 allocate(p2, source=p1)
 call do_allocate(p1, size(p1))
 allocate(p4, source=poly_init())

 if (lbound(p1, 1) /= 3 .or. ubound(p1, 1) /= 4 &
     .or. lbound(p2, 1) /= 3 .or. ubound(p2, 1) /= 4 &
     .or. lbound(p3, 1) /= 1 .or. ubound(p3, 1) /= 2 &
     .or. lbound(p4, 1) /= 1 .or. ubound(p4, 1) /= 2 &
     .or. p1(3)%i /= 43 .or. p1(4)%i /= 56 &
     .or. p2(3)%i /= 43 .or. p2(4)%i /= 56 &
     .or. p3(1)%i /= 43 .or. p3(2)%i /= 56 &
     .or. p4(1)%i /= 11 .or. p4(2)%i /= 12) then
   call abort()
 endif

 !write(*,*) lbound(a,1), ubound(a,1) ! prints 1 3
 !write(*,*) lbound(b,1), ubound(b,1) ! prints 1 3
 !write(*,*) lbound(c,1), ubound(c,1) ! prints 1 3
 !write(*,*) lbound(d,1), ubound(d,1) ! prints 1 5
 !write(*,*) lbound(e,1), ubound(e,1) ! prints 1 6

 if (lbound(a,1) /= 1 .or. ubound(a,1) /= 3 &
     .or. lbound(b,1) /= 1 .or. ubound(b,1) /= 3 &
     .or. lbound(c,1) /= 1 .or. ubound(c,1) /= 3 & 
     .or. lbound(d,1) /= 1 .or. ubound(d,1) /= 5 &
     .or. lbound(e,1) /= 1 .or. ubound(e,1) /= 6) then
   call abort()
 endif
   
contains

 subroutine do_allocate(x, n)
   integer, value :: n
   class(t), intent(in) :: x(n)
   allocate(p3, source=x)
 end subroutine

 function poly_init()
   class(t), allocatable :: poly_init(:)
   allocate(poly_init(7:8))
   poly_init = [t :: t(11), t(12)]
 end function poly_init

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
