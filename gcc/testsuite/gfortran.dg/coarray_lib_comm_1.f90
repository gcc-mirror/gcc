! { dg-do run }
! { dg-options "-fdump-tree-original -fcoarray=lib -lcaf_single" }
! { dg-additional-options "-latomic" { target libatomic_available } }
!
! Some dependency-analysis check for coarray communication
!
integer, target, save :: A(10)[*]
integer, pointer :: P(:)
integer, save :: B(10)[*]

A = [1,2,3,4,5,6,7,8,9,10]
B = [1,2,3,4,5,6,7,8,9,10]
A(10:2:-1) = A(9:1:-1)[1] ! 0
B(10:2:-1) = B(9:1:-1)
if (any (A-B /= 0)) STOP 1

A = [1,2,3,4,5,6,7,8,9,10]
B = [1,2,3,4,5,6,7,8,9,10]
A(9:1:-1) = A(10:2:-1)[1] ! 1
B(9:1:-1) = B(10:2:-1)
if (any (A-B /= 0)) STOP 2

A = [1,2,3,4,5,6,7,8,9,10]
B = [1,2,3,4,5,6,7,8,9,10]
allocate(P(10))
P(:) = A(:)[1] ! 1
if (any (A-B /= 0)) STOP 3

A = [1,2,3,4,5,6,7,8,9,10]
B = [1,2,3,4,5,6,7,8,9,10]
allocate(P(10))
P(:) = B(:)[1] ! 0

A = [1,2,3,4,5,6,7,8,9,10]
B = [1,2,3,4,5,6,7,8,9,10]
A(1:5)[1] = A(3:7)[1] ! 1
B(1:5) = B(3:7)
if (any (A-B /= 0)) STOP 4
end

! { dg-final { scan-tree-dump-times "_gfortran_caf_get_from_remote" 4 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_transfer_between_remotes" 1 "original" } }
! { dg-final { scan-tree-dump-not "_gfortran_caf_transfer_get" "original" } }
! { dg-final { scan-tree-dump-not "_gfortran_caf_transfer_send" "original" } }

