! { dg-do compile }
!
! PR 59143: [OOP] Bogus warning with array-valued type-bound procedure
!
! Contributed by Jürgen Reuter <juergen.reuter@desy.de>

module phs_single

  type :: phs_single_t
   contains
     procedure, nopass :: d1, d2
  end type
  
contains

  subroutine evaluate (phs)
    class(phs_single_t) :: phs
    call func1 (phs%d1 ())
    call func1 (phs%d2 (2))
  end subroutine

  subroutine func1 (p)
    real :: p(2)
  end subroutine
  
  function d1 ()
    real :: d1(2)
    d1 = 1.
  end function

  function d2 (n)
    real :: d2(n)
    d2 = 1.
  end function

end module

! { dg-final { cleanup-modules "phs_single" } }
