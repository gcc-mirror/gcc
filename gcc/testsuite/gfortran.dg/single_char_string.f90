! { dg-do run }
! { dg-options "-fdump-tree-original" }
! PR12456 - Optimize string(k:k) as single character.

Program pr12456
character a
character b
character (len=5) :: c
integer i

b = 'a'
a = b
if (a .ne. 'a') STOP 1
if (a .ne. b) STOP 2
c (3:3) = 'a'
if (c (3:3) .ne. b) STOP 3
if (c (3:3) .ne. 'a') STOP 4
if (LGT (a, c (3:3))) STOP 5
if (LGT (a, 'a')) STOP 6

i = 3
c (i:i) = 'a'
if (c (i:i) .ne. b) STOP 7
if (c (i:i) .ne. 'a') STOP 8
if (LGT (a, c (i:i))) STOP 9

if (a .gt. char (255)) STOP 10
end

! There should not be _gfortran_compare_string and _gfortran_copy_string in
! the dumped file.

! { dg-final { scan-tree-dump-times "_gfortran_compare_string" 0 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_copy_string" 0 "original" } }

