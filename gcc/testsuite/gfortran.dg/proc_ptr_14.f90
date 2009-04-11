! { dg-do compile }
! { dg-options "-std=f95" }
!
! PR 39692: f95: conflict between EXTERNAL and POINTER
!
! Test for Procedure Pointers (without PROCEDURE statements) with the -std=f95 flag.
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

pointer :: f
external :: f   ! { dg-error "Fortran 2003: Procedure pointer" }

external :: g
pointer :: g   ! { dg-error "Fortran 2003: Procedure pointer" }

real, pointer, external :: h   ! { dg-error "Fortran 2003: Procedure pointer" }

interface
  subroutine i
  end subroutine i
end interface
pointer :: i   ! { dg-error "Fortran 2003: Procedure pointer" }

pointer :: j
interface
  real function j()
  end function j   ! { dg-error "Fortran 2003: Procedure pointer" }
end interface

contains

  function k()   ! { dg-error "attribute conflicts with" }
    intrinsic sin
    external k
    pointer k   ! { dg-error "Fortran 2003: Procedure pointer" }
    real k
  end function k

end

