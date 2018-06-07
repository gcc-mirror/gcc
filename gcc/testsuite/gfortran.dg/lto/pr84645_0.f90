! { dg-lto-do link }
! { dg-lto-options { { -flto -g0 } } }
! { dg-extra-ld-options { -g } }
program nml_test
  implicit none
  type t
    integer :: c1
    integer :: c2(3)
  end type t
  call test2(2) 
contains
  subroutine test2(n)
    integer :: n
    type(t) :: x12(n)
    namelist /nml2/ x12
  end subroutine test2
end program nml_test
