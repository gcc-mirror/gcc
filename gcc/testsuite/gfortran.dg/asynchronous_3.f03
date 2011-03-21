! { dg-do compile }
!
! PR fortran/44457 - no array-subscript actual argument
!                    for an asynchronous dummy
!

  integer :: a(10), sect(3)
  sect = [1,2,3]
  call f(a(sect))    ! { dg-error "incompatible" }
  call f(a(::2))
contains
  subroutine f(x)
    integer, asynchronous :: x(:)
  end subroutine f
end
