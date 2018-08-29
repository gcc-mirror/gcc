! { dg-do run }
!
! PR 50570: [4.6/4.7 Regression] Incorrect error for assignment to intent(in) pointer
!
! Contributed by Bill Long <longb@cray.com>

program bots_sparselu_pointer_intent_in

  implicit none
  integer, pointer :: array(:)

  allocate(array(4))
  array = 0
  call sub(array)
  if (sum(array)/=1) STOP 1

contains

  subroutine sub(dummy)
    integer, pointer, intent(in) :: dummy(:)
    dummy(1) = 1
  end subroutine sub

end program
