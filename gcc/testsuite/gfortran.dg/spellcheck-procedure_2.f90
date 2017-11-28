! { dg-do compile }
! test levenshtein based spelling suggestions


program spellchekc
  implicit none (external) ! { dg-warning "GNU Extension: IMPORT NONE with spec list" }

  interface
    subroutine bark_unless_zero(iarg)
      implicit none
      integer, intent(in) :: iarg
    end subroutine bark_unless_zero
  end interface

  integer :: i
  i = 0

  if (i /= 1) call abort
  call bark_unless_0(i) ! { dg-error "not explicitly declared; did you mean .bark_unless_zero.\\?" }
!  call complain_about_0(i) ! { -dg-error "not explicitly declared; did you mean .complain_about_zero.\\?" }

contains
! We cannot reliably see this ATM, would need an unambiguous bit somewhere
  subroutine complain_about_zero(iarg)
    integer, intent(in) :: iarg
    if (iarg /= 0) call abort
  end subroutine complain_about_zero

end program spellchekc

subroutine bark_unless_zero(iarg)
  implicit none
  integer, intent(in) :: iarg
  if (iarg /= 0) call abort
end subroutine bark_unless_zero
