! { dg-do compile }
!
! PR fortran/31639
! Contributed by Martin Michlmayr <tbm AT cyrius DOT com>

   integer function xstrcmp(s1)
     character*(*), intent(in) :: s1
     integer :: n1 = len(s1)            ! { dg-error "Assumed character length variable" }
     n1 = 1
     return
   end function xstrcmp
