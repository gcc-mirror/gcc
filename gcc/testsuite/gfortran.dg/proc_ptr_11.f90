! { dg-do compile }
!
! PR 38290: Procedure pointer assignment checking.
!
! Test case found at http://de.wikibooks.org/wiki/Fortran:_Fortran_2003:_Zeiger
! Adapted by Janus Weil <janus@gcc.gnu.org>

program bsp
  implicit none   

  abstract interface
    subroutine up()
    end subroutine up
  end interface

  procedure( up ) , pointer :: pptr
  procedure(isign), pointer :: q

  procedure(iabs),pointer :: p1
  procedure(f), pointer :: p2

  pointer :: p3
  interface
    function p3(x)
      real(8) :: p3,x
    end function p3
  end interface

  pptr => add   ! { dg-error "Interfaces don't match" }

  q => add

  print *, pptr()   ! { dg-error "is not a function" }

  p1 => iabs
  p2 => iabs
  p1 => f
  p2 => f
  p2 => p1
  p1 => p2

  p1 => abs   ! { dg-error "Interfaces don't match" }
  p2 => abs   ! { dg-error "Interfaces don't match" }

  p3 => dsin
  p3 => sin   ! { dg-error "Interfaces don't match" }

  contains

    function add( a, b )
      integer               :: add
      integer, intent( in ) :: a, b
      add = a + b
    end function add

    integer function f(x)
      integer :: x
      f = 317 + x
    end function

end program bsp 
