! { dg-do run }
! PR 47065 - make sure that trim optimization does not lead to
! wrong-code with aliasing.
! Test case provided by Tobias Burnus.
program main
  character(len=12) :: str
  str = '1234567890'
  call sub(trim(str), str)
  ! Should print '12345       '
  if (str /= '12345       ') call abort
  call two(trim(str))
  if (str /= '123         ') call abort
contains
  subroutine sub(a,b)
    character(len=*), intent(in) :: a
    character(len=*), intent(out) :: b
    b = ''
    b = a(1:5)
  end subroutine sub
  subroutine two(a)
    character(len=*), intent(in) :: a
    str = ''
    str(1:3) = a(1:3)
  end subroutine two
end program main
