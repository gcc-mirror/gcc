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
if (a .ne. 'a') call abort()
if (a .ne. b) call abort()
c (3:3) = 'a'
if (c (3:3) .ne. b) call abort ()
if (c (3:3) .ne. 'a') call abort ()
if (LGT (a, c (3:3))) call abort ()
if (LGT (a, 'a')) call abort ()

i = 3
c (i:i) = 'a'
if (c (i:i) .ne. b) call abort ()
if (c (i:i) .ne. 'a') call abort ()
if (LGT (a, c (i:i))) call abort ()

if (a .gt. char (255)) call abort ()
end

! There should not be _gfortran_compare_string and _gfortran_copy_string in
! the dumped file.

! { dg-final { scan-tree-dump-times "_gfortran_compare_string" 0 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_copy_string" 0 "original" } }

! { dg-final { cleanup-tree-dump "original" } }
