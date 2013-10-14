! { dg-do compile }
!
! PR fortran/58658
!
! Contributed by VladimÃ­r Fuka
!
subroutine sub(a)
  class(*),allocatable :: a
  a => null() ! { dg-error "Non-POINTER in pointer association context \\(pointer assignment\\)" }
end subroutine
