! { dg-do run }
! Tests the fix for pr63553 in which the class container was being
! assigned to derived types, rather than the data.
!
! Contributed by <patnel97269-gfortran@yahoo.fr>
!
program toto
  implicit none
  type mother
    integer :: i
  end type mother
  type,extends(mother) :: child
  end type child

  call comment1
  call comment2

contains
  subroutine comment1
    type(mother) :: tm
    class(mother),allocatable :: cm

    allocate (cm)
    cm%i = 77
    tm = cm
    if (tm%i .ne. cm%i) STOP 1
  end subroutine

  subroutine comment2
    class(mother),allocatable :: cm,cm2

    allocate(cm)
    allocate(child::cm2)
    cm%i=10
    select type (cm2)
       type is (child)
         cm2%mother=cm
    end select
    if (cm2%i .ne. cm%i) STOP 2
  end subroutine
end program
