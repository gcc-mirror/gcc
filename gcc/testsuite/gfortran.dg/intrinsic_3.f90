! { dg-do compile }
! { dg-options "-std=f95" }
!
! PR 39876: module procedure name that collides with the GNU intrinsic
!
! Contributed by Alexei Matveev <alexei.matveev+gcc@gmail.com>

module p                           
  implicit none                                                                 

  contains

    subroutine test()
      implicit none
      print *, avg(erfc)
    end subroutine test

    function avg(f)
      implicit none
      double precision :: avg
      interface
        double precision function f(x)
          implicit none
          double precision, intent(in) :: x
        end function f
      end interface
      avg = ( f(1.0D0) + f(2.0D0) ) / 2
    end function avg

    function erfc(x)
      implicit none
      double precision, intent(in) :: x
      double precision             :: erfc
      erfc = x
    end function erfc

end module p

! { dg-final { cleanup-modules "p" } }

