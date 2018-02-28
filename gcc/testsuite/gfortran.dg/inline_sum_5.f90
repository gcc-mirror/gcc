! { dg-do run }
!
! PR fortran/57798
! The call to sum used to be inlined into a loop with an uninitialized bound
!
! Original testcase by Stephan Kramer <stephan.kramer@imperial.ac.uk>

program test
  implicit none

  call sub(2, 11)

  contains

    function func(m, n)
      integer, intent(in):: m,n
      real, dimension(m, n):: func

      func = 1.0

    end function func

    subroutine sub(m, n)
      integer, intent(in):: m, n
      real, dimension(m,n):: y

      y = 1.0
      if (any(sum(y*func(m,n), dim=1) /= m)) STOP 1

    end subroutine sub

end program test

