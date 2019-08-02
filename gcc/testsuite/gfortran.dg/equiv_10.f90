! { dg-do compile }
! PR fortran/90986
module mymod
  type :: mytyp
    integer :: i
  end type mytyp
contains
  subroutine mysub
    implicit none
    type(mytyp) :: a
    integer :: equivalencei,equivalencej
    equivalencei = a%i
    equivalencej = a%j  ! { dg-error "is not a member of the" }
  end subroutine mysub
end module mymod
