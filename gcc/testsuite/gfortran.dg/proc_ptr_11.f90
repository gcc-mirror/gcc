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

  ! TODO. See PR 38290.
  !pptr => add   ! { "Interfaces don't match" }

  q => add

  print *, pptr()   ! { dg-error "is not a function" }

  contains

    function add( a, b )
      integer               :: add
      integer, intent( in ) :: a, b
      add = a + b
    end function add

end program bsp 
