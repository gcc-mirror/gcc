! { dg-do compile }
!
! PR fortran/92736
!
! Contributed by Chinoune Mehdi
!
module m1
  implicit none
  integer, parameter :: i = 10
end module m1

module m2
  use m1, only : i
  implicit none
  interface
    module subroutine sb1()
    end subroutine sb1
  end interface
end module m2

submodule(m2) s1
  use m1, only : i
  implicit none
contains
  module subroutine sb1
    print *,"hello", i
  end subroutine sb1
end submodule s1
