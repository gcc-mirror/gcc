! { dg-do run }
!
! The handling of scalar optional arguments passed to elemental procedure
! did not keep actual arguments and dummy arguments synchronized while
! walking them in gfc_walk_elemental_function_args, leading to a
! null pointer dereference in the generated code.
!
  implicit none

  integer, parameter :: n = 3

  call do_test

contains

  elemental function five(nonopt1, opt1, nonopt2, opt2)
    integer, intent(in), optional :: opt1, opt2
    integer, intent(in) :: nonopt1, nonopt2
    integer :: five

    if (.not. present(opt1) .and. .not. present(opt2)) then
      five = 5
    else
      five = -7
    end if
  end function five

  subroutine do_test(opt)
    integer, optional :: opt
    integer :: i = -1, a(n) = (/ (i, i=1,n) /)
    integer :: b(n)

    b = five(a, nonopt2=i, opt2=opt)
    if (any(b /= 5)) STOP 1
  end subroutine do_test

end
