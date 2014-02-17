! PR fortran/52370
! { dg-do compile }
! { dg-options "-O1 -Wall" }

module pr52370
contains
  subroutine foo(a,b)
    real, intent(out) :: a
    real, dimension(:), optional, intent(out) :: b
    a=0.5
    if (present(b)) then
      b=1.0
    end if
  end subroutine foo
end module pr52370

program prg52370
  use pr52370
  real :: a
  call foo(a)
end program prg52370
