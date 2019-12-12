! { dg-do compile }
module mymod
  type :: mytyp
    integer :: i
  end type mytyp
contains
  subroutine mysub
    implicit none
    type(mytyp) :: a
    integer :: commoni,commonj
    commoni = a%i
    commonj = a%j             ! { dg-error "is not a member of" }
  end subroutine mysub
end module mymod
