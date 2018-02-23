! PR13930
! We were trying to assign a default initializer to dummy variables.
program der_init_4
  type t
    integer :: i = 42
  end type

  call foo(t(5))
contains
subroutine foo(a)
  type (t), intent(in) :: a

  if (a%i .ne. 5) STOP 1
end subroutine
end program
