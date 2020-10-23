! { dg-do run }
! { dg-options "-fdump-tree-original" }
! { dg-final { scan-tree-dump-times "string_index" 0 "original" } }
! PR fortran/95979

program p
  implicit none
  integer, parameter :: i0    = index( 'abcd',  'b' , .true. , kind=4)
  integer, parameter :: i1(*) = index(['abcd'], 'b' , .true. , kind=4)
  integer, parameter :: i2(*) = index( 'abcd' ,['b'], .true.         )
  integer, parameter :: i3(*) = index( 'abcd' , 'b' ,[.true.]        )
  integer, parameter :: i4(*) = index(['abcd'],['b'],[.true.], kind=8)
  if (size (i1) /= 1) stop 1
  if (size (i2) /= 1) stop 2
  if (size (i3) /= 1) stop 3
  if (size (i4) /= 1) stop 4
  if (i0 /= 2)        stop 5
  if (i1(1) /= 2 .or. i2(1) /= 2 .or. i3(1) /= 2 .or. i4(1) /= 2) stop 6
end
