! { dg-do run }
! See attachment to PR82173 comment 10 - reproducers/bug7-parent-default-init.f90
program h1
  type :: plain
    integer :: id = 11
    real :: weight = 0.5
  end type
  type, extends(plain) :: para(n)
    integer, len :: n = 1
    real :: v(n)
  end type
  type(para(3)) :: p
  type(plain) :: q
  if (p%id /= 11) stop 1                       ! Inherited values 
  if (int (200.0 * p%weight) /= 100) stop 2    ! were zero
  if (q%id /= 11) stop 3 
  if (int (200.0 * q%weight) /= 100) stop 4
end program
