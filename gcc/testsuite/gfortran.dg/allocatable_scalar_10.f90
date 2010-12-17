! { dg-do run }
!
! PR 42647: Missed initialization/dealloc of allocatable scalar DT with allocatable component
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

type t
 integer, allocatable :: p
end type t
type(t), allocatable :: a

deallocate(a,stat=istat)
if (istat == 0) call abort()
end 
