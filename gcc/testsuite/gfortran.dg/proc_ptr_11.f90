! { dg-do compile }
!
! PR 38290: Procedure pointer assignment checking.
!
! Test case found at http://de.wikibooks.org/wiki/Fortran:_Fortran_2003:_Zeiger
! Adapted by Janus Weil <janus@gcc.gnu.org>

program bsp
  implicit none   
  intrinsic :: isign, iabs
  abstract interface
    subroutine up()
    end subroutine up
    ! As intrinsics but not elemental
    pure integer function isign_interf(a, b)
       integer, intent(in) :: a, b
    end function isign_interf
    pure integer function iabs_interf(x)
       integer, intent(in) :: x
    end function iabs_interf
  end interface

  procedure( up ) , pointer :: pptr
  procedure(isign_interf), pointer :: q

  procedure(iabs_interf),pointer :: p1
  procedure(f), pointer :: p2

  pointer :: p3
  interface
    function p3(x)
      real(8) :: p3,x
      intent(in) :: x
    end function p3
  end interface

  pptr => add   ! { dg-error "is not a subroutine" }

  q => add

  print *, pptr()   ! { dg-error "is not a function" }

  p1 => iabs
  p2 => iabs
  p1 => f
  p2 => f
  p2 => p1
  p1 => p2

  p1 => abs   ! { dg-error "Type mismatch in function result" }
  p2 => abs   ! { dg-error "Type mismatch in function result" }

  p3 => dsin
  p3 => sin   ! { dg-error "Type mismatch in function result" }

  contains

    pure function add( a, b )
      integer               :: add
      integer, intent( in ) :: a, b
      add = a + b
    end function add

    pure integer function f(x)
      integer,intent(in) :: x
      f = 317 + x
    end function

end program bsp 
