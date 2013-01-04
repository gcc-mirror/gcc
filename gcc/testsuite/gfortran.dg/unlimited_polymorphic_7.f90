! { dg-do compile }
!
! PR fortran/55763
!
! Contributed by Harald Anlauf
!

module gfcbug121
  implicit none
  type myobj
     class(*), allocatable :: x
   contains
     procedure :: print
  end type myobj
contains
  subroutine print(this)
    class(myobj) :: this
  end subroutine print
end module gfcbug121
