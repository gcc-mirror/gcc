! { dg-do compile }
module mymod
  type :: mytyp
    integer :: i
  end type mytyp
contains
  subroutine mysub
    implicit none
    type(mytyp) :: a
    integer :: privatei,privatej
    privatei = a%i
    privatej = a%j       ! { dg-error "is not a member" }
  end subroutine mysub
end module mymod
