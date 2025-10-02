! { dg-do run }
!
! Test the fix for P83746, which failed as in the comment below.
!
! Contributed by Berke Durak  <berke.durak@gmail.com>
!
module pdt_m
  implicit none
  type :: vec(k)
     integer, len :: k
     integer :: foo(k)
  end type vec
contains
  elemental function diy_max(a,b) result(c)
    integer, intent(in) :: a,b
    integer :: c
    c=max(a,b)
  end function diy_max

  function add(a,b) result(c)
    type(vec(k=*)), intent(in) :: a,b
    type(vec(k=max(a%k,b%k))) :: c      ! Fails
    !type(vec(k=diy_max(a%k,b%k))) :: c ! Worked with diy_max
    !type(vec(k=a%k+b%k)) :: c          ! Worked with +

    c%foo(1:a%k)=a%foo
    c%foo(a%k+1:) = 0
    c%foo(1:b%k)=c%foo(1:b%k)+b%foo

    if (c%k /= 5) stop 1
  end function add
end module pdt_m

program test_pdt
  use pdt_m
  implicit none
  type(vec(k=2)) :: u
  type(vec(k=5)) :: v,w

  if (w%k /= 5) stop 2
  if (size(w%foo) /= 5) stop 3

  u%foo=[1,2]
  v%foo=[10,20,30,40,50]
  w=add(u,v)

  if (w%k /= 5) stop 4
  if (size(w%foo) /= 5) stop 5
  if (any (w%foo /= [11,22,30,40,50])) stop 6
end program test_pdt
