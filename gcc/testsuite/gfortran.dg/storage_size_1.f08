! { dg-do run }
!
! PR 44649: [OOP] F2008: storage_size intrinsic
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

type :: t
  integer(4) :: i
  real(4) :: r
end type

type,extends(t) :: t2
  integer(4) :: j
end type

type(t) :: a
type(t), dimension(1:3) :: b
class(t), allocatable :: cp

allocate(t2::cp)

if (sizeof(a)        /=  8) call abort()
if (storage_size(a)  /= 64) call abort()

if (sizeof(b)        /= 24) call abort()
if (storage_size(b)  /= 64) call abort()

if (sizeof(cp)       /=  8) call abort()
if (storage_size(cp) /= 96) call abort()

end
