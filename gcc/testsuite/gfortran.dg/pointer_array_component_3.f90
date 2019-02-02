! { dg-do run }
!
! Test the fix for PR88685, in which the component array references in 'doit'
! were being ascribed to the class pointer 'Cls' itself so that the stride
! measure between elements was wrong.
!
! Contributed by Antony Lewis  <antony@cosmologist.info>
!
program tester
  implicit none
  Type TArr
    integer, allocatable :: CL(:)
  end Type TArr

  type(TArr), allocatable, target :: arr(:,:)
  class(TArr), pointer:: Cls(:,:)
  integer i

  allocate(arr(1,1))
  allocate(arr(1,1)%CL(3))
  arr(1,1)%CL=-1
  cls => arr
  call doit(cls)
  if (any (arr(1,1)%cl .ne. [3,2,1])) stop 3
contains
  subroutine doit(cls)
    class(TArr), pointer :: Cls(:,:)

    cls(1,1)%CL(1) = 3
    cls(1,1)%CL(2:3) = [2,1]

    if (any (Cls(1,1)%CL .ne. [3,2,1])) stop 1
    if (Cls(1,1)%CL(2) .ne. 2) stop 2

  end subroutine doit
end program tester
