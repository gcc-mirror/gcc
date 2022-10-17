! { dg-do compile }
! PR fortran/103609 - ICE in gfc_sym_get_dummy_args
! Contributed by G.Steinmetz

program p
  implicit none
  integer :: i
  do i = 1, 2
     call s
  end do
contains
  subroutine s
    call sub(x) ! { dg-error "has no IMPLICIT type" }
  end
end
