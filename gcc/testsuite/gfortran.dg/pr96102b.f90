! { dg-do compile }
!
! PR fortran/108544 - host association
! Variation of testcase pr96102.f90 using subroutines instead of functions

module m
  type mytype
    integer :: i
  end type
  type(mytype) :: d = mytype (42) ! { dg-error "is host associated" }
  integer      :: n = 2           ! { dg-error "is host associated" }
contains
  subroutine s
    if ( n   /= 0 ) stop 1  ! { dg-error "internal procedure of the same name" }
    if ( d%i /= 0 ) stop 2  ! { dg-error "internal procedure of the same name" }
  contains
    subroutine n()
    end
    subroutine d()
    end
  end
end

! { dg-prune-output "Operands of comparison operator" }
