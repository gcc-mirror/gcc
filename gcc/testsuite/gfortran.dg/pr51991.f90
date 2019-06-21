! { dg-do compile }
! PR fortran/51991
! Orginal code contributed by Sebastien Bardeau <bardeau at iram dot fr>
module mymod
  type :: mytyp
    integer :: i
  end type mytyp
contains
  subroutine mysub
    implicit none
    type(mytyp) :: a
    integer :: i,j
    i = a%i
    !
    ! Prior to patching gfortran, the following lined generated a syntax
    ! error with the SAVE statement.  Now, gfortran generates an error
    ! that indicates 'j' is not a component of 'mytyp'.
    !
    j = a%j    ! { dg-error "is not a member of the" }
  end subroutine mysub
end module mymod
