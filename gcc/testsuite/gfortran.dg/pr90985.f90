! { dg-do compile }
module mymod
  type :: mytyp
    integer :: i
  end type mytyp
contains
  subroutine mysub
    implicit none
    type(mytyp) :: a
    integer :: datai,dataj
    datai = a%i
    dataj = a%j         ! { dg-error "is not a member of the" }
  end subroutine mysub
end module mymod
