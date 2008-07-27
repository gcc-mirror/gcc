! { dg-do run }
! { dg-options "-fcheck-array-temporaries" }
 program test
  implicit none
  integer :: a(3,3)
  call foo(a(:,1))  ! OK, no temporary created
  call foo(a(1,:))  ! BAD, temporary var created
contains
  subroutine foo(x)
    integer :: x(3)
    x = 5
  end subroutine foo
end program test

! { dg-output "At line 7 of file .*array_temporaries_2.f90(\n|\r\n|\r)Fortran runtime warning: An array temporary was created for argument 'x' of procedure 'foo'" }
