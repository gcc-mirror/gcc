! { dg-do run }
! Test the fix for PR38852 and PR39006 in which LBOUND did not work
! for some arrays with negative strides.
!
! Contributed by Dick Hendrickson  <dick.hendrickson@gmail.com>
!                Clive Page        <clivegpage@googlemail.com>
!            and Mikael Morin      <mikael.morin@tele2.fr>
!
program try_je0031
  integer ida(4)
  real dda(5,5,5,5,5)
  integer, parameter :: nx = 4, ny = 3
  interface
    SUBROUTINE PR38852(IDA,DDA,nf2,nf5,mf2)
      INTEGER IDA(4)
      REAL DDA(5,5,5,5,5)
      TARGET DDA
    END SUBROUTINE
  end interface
  integer :: array1(nx,ny), array2(nx,ny) 
  data array2 / 1,2,3,4, 10,20,30,40, 100,200,300,400 /
  array1 = array2
  call PR38852(IDA,DDA,2,5,-2)
  call PR39006(array1, array2(:,ny:1:-1))
  call mikael         ! http://gcc.gnu.org/ml/fortran/2009-01/msg00342.html
contains
  subroutine PR39006(array1, array2)
    integer, intent(in) :: array1(:,:), array2(:,:)
    integer :: j
    do j = 1, ubound(array2,2)
      if (any (array1(:,j) .ne. array2(:,4-j))) call abort
    end do
  end subroutine
end 

SUBROUTINE PR38852(IDA,DDA,nf2,nf5,mf2)
  INTEGER IDA(4)
  REAL DLA(:,:,:,:)
  REAL DDA(5,5,5,5,5)
  POINTER DLA
  TARGET DDA
  DLA => DDA(2:3, 1:3:2, 5:4:-1, NF2, NF5:NF2:MF2)
  IDA = UBOUND(DLA)
  if (any(ida /= 2)) call abort
  DLA => DDA(2:3, 1:3:2, 5:4:-1, 2, 5:2:-2)
  IDA = UBOUND(DLA)
  if (any(ida /= 2)) call abort
!
! These worked.
!
  DLA => DDA(2:3, 1:3:2, 5:4:-1, 2, 5:2:-2)
  IDA = shape(DLA)
  if (any(ida /= 2)) call abort
  DLA => DDA(2:3, 1:3:2, 5:4:-1, 2, 5:2:-2)
  IDA = LBOUND(DLA)
  if (any(ida /= 1)) call abort
END SUBROUTINE

subroutine mikael
  implicit none
  call test (1,  3, 3)
  call test (2,  3, 3)
  call test (2, -1, 0)
  call test (1, -1, 0)
contains
  subroutine test (a, b, expect)
    integer :: a, b, expect
    integer :: c(a:b)
    if (ubound (c, 1) .ne. expect) call abort
  end subroutine test
end subroutine
