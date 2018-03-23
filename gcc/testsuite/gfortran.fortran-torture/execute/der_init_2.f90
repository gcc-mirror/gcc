! PR 15314
! We were looking at the type of the initialization expression, not the type
! of the field.
program der_init_2
  implicit none
  type foo
    integer :: a(3) = 42
    integer :: b = 123
  end type

  type (foo) :: v

  if ((v%b .ne. 123) .or. any (v%a .ne. 42)) STOP 1;
end program

