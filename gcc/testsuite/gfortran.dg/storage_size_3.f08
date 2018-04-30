! { dg-do run }
!
! PR 47024: [OOP] STORAGE_SIZE (for polymorphic types): Segfault at run time
! PR 47189: [OOP] calling STORAGE_SIZE on a NULL-initialized class pointer
! PR 47194: [OOP] EXTENDS_TYPE_OF still returns the wrong result if the polymorphic variable is unallocated
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

type t
  integer(kind=4) :: a
end type

class(t), pointer :: x => null()
class(t), allocatable :: y

if (storage_size(x)/=32) STOP 1
if (storage_size(y)/=32) STOP 2

allocate(y)

if (storage_size(y)/=32) STOP 3

deallocate(y)

if (storage_size(y)/=32) STOP 4

end 
