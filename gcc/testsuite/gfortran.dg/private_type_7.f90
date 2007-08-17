! { dg-do compile }
! PR32760 Error defining subroutine named PRINT
! Test case derived from original PR.

module gfcbug68
  implicit none
  private :: write

contains

  function foo (i)
    integer, intent(in)  :: i
    integer foo

    write (*,*) i
    call write(i)
    foo = i
  end function foo

  subroutine write (m)
    integer, intent(in) :: m
    print *, m*m*m
  end subroutine write

end module gfcbug68

program testit
  use gfcbug68
  integer :: i = 27
  integer :: k
  
  k = foo(i)
  print *, "in the main:", k
end program testit
! { dg-final { cleanup-modules "gfcbug68" } }
