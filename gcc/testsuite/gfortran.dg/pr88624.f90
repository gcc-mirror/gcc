!{ dg-do compile }
!{ dg-options "-fcoarray=lib" }

! Check that PR fortran/88624 is fixed.
! Contributed by Modrzejewski  <m.modrzejewski@student.uw.edu.pl>
! Reduced to the essence of the issue.

program test 
      implicit none 
      integer, dimension(:), allocatable :: x[:] 
      call g(x) 
contains 
      subroutine g(x) 
            integer, dimension(:), allocatable :: x[:] 
            call g2(x) 
      end subroutine g 
      subroutine g2(x) 
            integer, dimension(:) :: x[*] 
      end subroutine g2 
end program test 

