! { dg-do run }
! { dg-options "-fcheck=bounds -fdump-tree-original" }
! PR fortran/98490 - out of bounds in array constructor with implied do loop

program test
  implicit none
  call sub('Lorem ipsum')
  call sub2('Lorem ipsum')
  call sub3('Lorem ipsum')
contains
  subroutine sub( text )
    character(len=*), intent(in)  :: text
    character(len=1), allocatable :: c(:)
    integer :: i
    c = [ ( text(i:i), i = 1, len(text) ) ]
    if (c(1) /= 'L') stop 1
  end subroutine sub
  subroutine sub2 (txt2)
    character(len=*), intent(in)  :: txt2
    character(len=1), allocatable :: c(:)
    integer :: i
    c = [ ( txt2(i+0:i), i = 1, len(txt2) ) ]
    if (c(1) /= 'L') stop 2
  end subroutine sub2
  subroutine sub3 (txt3)
    character(len=*), intent(in)  :: txt3
    character(len=1), allocatable :: c(:)
    integer :: i
    c = [ ( txt3(i:i+0), i = 1, len(txt3) ) ]
    if (c(1) /= 'L') stop 3
  end subroutine sub3
end program test

! { dg-final { scan-tree-dump-times "Substring out of bounds:" 6 "original" } }
