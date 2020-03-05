! { dg-do run { target c99_runtime } }
! { dg-additional-sources ISO_Fortran_binding_13.c }
!
! Test the fix for PR91926. The additional source is the main program.
!
! Contributed by José Rui Faustino de Sousa  <jrfsousa@hotmail.com>
!
program ifb_p

  implicit none

  integer :: i = 42

  interface
    integer function ifb_echo_aux(this) bind(c, name="ifb_echo")
      implicit none
      type(*), dimension(..), & ! removing assumed rank solves segmentation fault
        optional, intent(in) :: this
    end function ifb_echo_aux
  end interface

  if (ifb_echo_aux() .ne. 1) STOP 1  ! worked
  if (ifb_echo() .ne. 1) stop 2      ! segmentation fault
  if (ifb_echo_aux(i) .ne. 2) stop 3 ! worked
  if (ifb_echo(i) .ne. 2) stop 4     ! worked

  stop

contains

  integer function ifb_echo(this)
    type(*), dimension(..), &
      optional, intent(in) :: this

    ifb_echo = ifb_echo_aux(this)
    return
  end function ifb_echo

end program ifb_p
