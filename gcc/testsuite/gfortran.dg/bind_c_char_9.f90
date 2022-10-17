! { dg-do run }
! { dg-additional-options "-fdump-tree-original" }

! F2018  - examples without array descriptor


module m
  use iso_c_binding, only: c_char
  implicit none (type, external)

contains

! Scalar, nonallocatable/nonpointer
subroutine s1 (x1) bind(C)
  character(kind=c_char, len=1) :: x1
  if (len (x1) /= 1) stop
  if (x1 /= 'Z') stop
  x1 = 'A'
end

! Valid as Fortran code - but with BIND(C)
! 18.3.6 (5) (bullet 5) requires interoperability, i.e. len=1
! which is not fullfilled.
!
! [It would work as with len=<const> the length is known
!  and only a bytestream is passed around.]
!subroutine s2 (x2) bind(C)
!  character(kind=c_char, len=2) :: x2
!  if (len (x2) /= 2) stop
!  if (x2 /= '42') stop
!  x2 = '64'
!end

! Assumed-size array, nonallocatable/nonpointer

subroutine az1 (x1) bind(C)
  character(kind=c_char, len=1) :: x1(*)
  if (len(x1) /= 1) stop  
  if (any (x1(:6) /= ['g', &
                      'd', &
                      'f', &
                      's', &
                      '3', &
                      '5'])) stop 1
  x1(:6) = ['1', &
            'h', &
            'f', &
            '3', &
            '4', &
            'h']
end

! Valid as Fortran code - but with BIND(C)
! 18.3.6 (5) (bullet 5) requires interoperability, i.e. len=1
! which is not fullfilled.
!
! [It would work as with len=<const> the length is known
!  and only a bytestream is passed around.]
!subroutine az2 (x2) bind(C)
!  character(kind=c_char, len=2) :: x2(*)
!  if (len(x2) /= 2) stop  
!  if (any (x2(:6) /= ['ab', &
!                      'fd', &
!                      'D4', &
!                      '54', &
!                      'ga', &
!                      'hg'])) stop
!  x2(:6) = ['ab', &
!            'hd', &
!            'fj', &
!            'a4', &
!            '4a', &
!            'hf']
!end

! Explicit-size array, nonallocatable/nonpointer

subroutine ae1 (x1) bind(C)
  character(kind=c_char, len=1) :: x1(6)
  if (size(x1) /= 6) stop
  if (len(x1) /= 1) stop  
  if (any (x1 /= ['g', &
                  'd', &
                  'f', &
                  's', &
                  '3', &
                  '5'])) stop 1
  x1 = ['1', &
        'h', &
        'f', &
        '3', &
        '4', &
        'h']
end

! Valid as Fortran code - but with BIND(C)
! 18.3.6 (5) (bullet 5) requires interoperability, i.e. len=1
! which is not fullfilled.
!
! [It would work as with len=<const> the length is known
!  and only a bytestream is passed around.]
!subroutine ae2 (x2) bind(C)
!  character(kind=c_char, len=2) :: x2(6)
!  if (size(x2) /= 6) stop
!  if (len(x2) /= 2) stop  
!  if (any (x2 /= ['ab', &
!                  'fd', &
!                  'D4', &
!                  '54', &
!                  'ga', &
!                  'hg'])) stop
!  x2 = ['ab', &
!        'hd', &
!        'fj', &
!        'a4', &
!        '4a', &
!        'hf']
!end

end module m

program main
  use m
  implicit none (type, external)
  character(kind=c_char, len=1) :: str1
  character(kind=c_char, len=2) :: str2

  character(kind=c_char, len=1) :: str1a6(6)
  character(kind=c_char, len=2) :: str2a6(6)

  ! Scalar - no array descriptor

  str1 = 'Z'
  call s1 (str1)
  if (str1 /= 'A') stop

!  str2 = '42'
!  call s2 (str2)
!  if (str2 /= '64') stop

  ! assumed size - without array descriptor

  str1a6 = ['g', &
            'd', &
            'f', &
            's', &
            '3', &
            '5']
  call az1 (str1a6)
  if (any (str1a6 /= ['1', &
                      'h', &
                      'f', &
                      '3', &
                      '4', &
                      'h'])) stop
!  str2a6 = ['ab', &
!            'fd', &
!            'D4', &
!            '54', &
!            'ga', &
!            'hg']
!  call az2 (str2a6)
!  if (any (str2a6 /= ['ab', &
!                      'hd', &
!                      'fj', &
!                      'a4', &
!                      '4a', &
!                      'hf'])) stop

  ! explicit size - without array descriptor

  str1a6 = ['g', &
            'd', &
            'f', &
            's', &
            '3', &
            '5']
  call ae1 (str1a6)
  if (any (str1a6 /= ['1', &
                      'h', &
                      'f', &
                      '3', &
                      '4', &
                      'h'])) stop
!  str2a6 = ['ab', &
!            'fd', &
!            'D4', &
!            '54', &
!            'ga', &
!            'hg']
!  call ae2 (str2a6)
!  if (any (str2a6 /= ['ab', &
!                      'hd', &
!                      'fj', &
!                      'a4', &
!                      '4a', &
!                      'hf'])) stop
end

! All argument shall be passed without descriptor
! { dg-final { scan-tree-dump-not "dtype" "original" } }
! { dg-final { scan-tree-dump-times "void s1 \\(character\\(kind=1\\)\\\[1:1\\\] & restrict x1\\)" 1 "original" } }
! { dg-final { scan-tree-dump-not "void s2 " "original" } }
! { dg-final { scan-tree-dump-times "void az1 \\(character\\(kind=1\\)\\\[0:\\\]\\\[1:1\\\] \\* restrict x1\\)" 1 "original" } }
! { dg-final { scan-tree-dump-not "void az2 " "original" } }
! { dg-final { scan-tree-dump-times "void ae1 \\(character\\(kind=1\\)\\\[6\\\]\\\[1:1\\\] \\* restrict x1\\)" 1 "original" } }
! { dg-final { scan-tree-dump-not "void ae2 " "original" } }
