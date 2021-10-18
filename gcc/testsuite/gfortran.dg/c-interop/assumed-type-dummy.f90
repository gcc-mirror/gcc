! PR 101319
! { dg-do compile }
!
! TS 29113
! 6.3 Argument association
!
! An assumed-type dummy argument shall not correspond to an actual argument
! that is of a derived type that has type parameters, type-bound procedures,
! or final subroutines.
!
! In the 2018 Fortran standard, this requirement appears as:
!
! 15.5.2.4 Ordinary dummy variables
!
! If the actual argument is of a derived type that has type parameters,
! type-bound procedures, or final subroutines, the dummy argument shall 
! not be assumed-type.
!
! This file contains code that is expected to produce errors.

module m

  ! basic derived type
  type :: t1
    real*8 :: xyz (3)
  end type

  ! derived type with type parameters
  type t2 (k, l)
    integer, kind :: k
    integer, len :: l
    real(k) :: a(l)
  end type

  ! derived type with a type-bound procedure
  type :: t3
    integer :: xyz(3)
    contains
      procedure, pass :: frob => frob_t3
  end type

  ! derived type with a final subroutine
  type :: t4
    integer :: xyz(3)
    contains
      final :: final_t4
  end type

contains

  ! implementation of the type-bound procedure for t3 above
  subroutine frob_t3 (a)
    class (t3) :: a
    a%xyz = 0
  end subroutine

  ! implementation of the final subroutine for t4 above
  subroutine final_t4 (a)
    type (t4) :: a
    a%xyz = 0
  end subroutine

  ! useless subroutine with an assumed-type dummy.
  subroutine s1 (a)
    type(*) :: a
  end subroutine

  ! test procedure
  subroutine testit
    type(t1) :: a1
    type(t2(8,20)) :: a2
    type(t3) :: a3
    type(t4) :: a4

    call s1 (a1)  ! OK
    call s1 (a2)  ! { dg-error "assumed-type dummy" }
    call s1 (a3)  ! { dg-error "assumed-type dummy" }
    call s1 (a4)  ! { dg-error "assumed-type dummy" }
  end subroutine

end module



