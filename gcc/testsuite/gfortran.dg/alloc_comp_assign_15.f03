! { dg-do run }
!
! Check the test for PR69422, in which the allocatable component 'Source'
! of the pointer component 'P' was not automatically (re)allocated on
! assignment.
!
! Contributed by Anthony Lewis  <antony@cosmologist.info>
!
module funcs
  implicit none

  Type T
    character(LEN=:), allocatable :: source
  end type T

  type TPointer
    Type(T), pointer :: P
  end type TPointer

end module

program Test1
  use funcs
  Type(TPointer) :: X

  allocate(X%P)

  X%P%Source = 'test string'
  if (.not.allocated (X%P%Source)) STOP 1
  if (X%P%Source .ne. 'test string') STOP 2

end program Test1
