! { dg-do run }
! { dg-options "-fcheck=bounds -fdump-tree-original" }
! PR fortran/98490 - out of bounds in array constructor with implied do loop

program test
  implicit none
  call sub('Lorem ipsum')
contains
  subroutine sub( text )
    character(len=*), intent(in)  :: text
    character(len=1), allocatable :: c(:)
    integer :: i
    c = [ ( text(i:i), i = 1, len(text) ) ]
    if (c(1) /= 'L') stop 1
  end subroutine sub
end program test

! { dg-final { scan-tree-dump-times "Substring out of bounds:" 2 "original" } }
