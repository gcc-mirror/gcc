! { dg-do compile }
!
! PR fortran/55763
!
! Contributed by Harald Anlauf
!

module gfcbug122
  implicit none
  type myobj
     class(*), allocatable :: x
   contains
     procedure :: print
  end type myobj
contains
  subroutine print(this)
    class(myobj) :: this
    select type (this)
    type is (integer) ! { dg-error "Unexpected intrinsic type 'INTEGER'" }
    type is (real) ! { dg-error "Unexpected intrinsic type 'REAL'" }
    type is (complex) ! { dg-error "Unexpected intrinsic type 'COMPLEX'" }
    type is (character(len=*)) ! { dg-error "Unexpected intrinsic type 'CHARACTER'" }
    end select
  end subroutine print
end module gfcbug122
