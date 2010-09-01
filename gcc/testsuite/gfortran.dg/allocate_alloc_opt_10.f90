! { dg-do run }
!
! PR 43388: [F2008][OOP] ALLOCATE with MOLD=
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

type :: t1
  integer :: i
end type

type,extends(t1) :: t2
  integer :: j = 4
end type

class(t1),allocatable :: x,y
type(t2) :: z


!!! first example (static)

z%j = 5
allocate(x,MOLD=z)

select type (x)
type is (t2)
  print *,x%j
  if (x%j/=4) call abort
  x%j = 5
class default
  call abort()
end select


!!! second example (dynamic, PR 44541)

allocate(y,MOLD=x)

select type (y)
type is (t2)
  print *,y%j
  if (y%j/=4) call abort
class default
  call abort()
end select

end
