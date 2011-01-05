! { dg-do run }
!
! PR 47024: [OOP] STORAGE_SIZE (for polymorphic types): Segfault at run time
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

type t
  integer(kind=4) :: a
end type
class(t), allocatable :: y
if (storage_size(y)/=32) call abort()
end 
