! { dg-do compile }
! Test fix for regression caused by 
! 2006-06-23  Steven G. Kargl  <kargls@comcast.net>
!    PR fortran/27981
!    * match.c (gfc_match_if):  Handle errors in assignment in simple if.
!
module read
  integer i, j, k
  contains
    subroutine a
      integer, parameter :: n = 2
      if (i .eq. 0) read(j,*) k
      if (i .eq. 0) n = j    ! { dg-error "Named constant 'n' in variable definition context" }
    end subroutine a
end module read
