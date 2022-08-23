! { dg-do compile }
! PR fortran/100892 - procedure pointer to function returning array of size n 

module m
  implicit none
  procedure(func1), pointer :: my_ptr => null()
contains
  subroutine test_sub
    if (associated (my_ptr, func1)) print *,'associated'
  end subroutine test_sub
  function func1 (n)
    integer, intent(in) :: n
    real, dimension(n)  :: func1
    func1 = 0.
  end function
end module m
