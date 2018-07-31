! { dg-do compile }
!
! PR 85088: improve diagnostic for bad INTENT declaration
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

subroutine s(x, y, z)
   integer, intent(int) :: x  ! { dg-error "Bad INTENT specification" }
   integer, intent :: y       ! { dg-error "Bad INTENT specification" }
   integer, inten  :: z       ! { dg-error "Invalid character" }
end
