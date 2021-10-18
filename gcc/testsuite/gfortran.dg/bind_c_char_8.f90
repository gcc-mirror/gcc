! { dg-do compile }
! { dg-additional-options "-fimplicit-none" }

! F2018 only permittes len=*, len=: or len=<const> as dummy argument
! but not len=<non-const-expr>
! Additionally, for allocatable/pointer, len=: is required.

! Scalar, nonallocatable/nonpointer

subroutine val_s1(x1) bind(C)
  character(len=1), value :: x1
end

subroutine val_s2(x2) bind(C) ! { dg-error "Character dummy argument 'x2' at .1. must be of length 1 as it has the VALUE attribute" }
  character(len=2), value :: x2
end

subroutine s1 (x1) bind(C)
  character(len=1) :: x1
end

subroutine s2 (x2) bind(C) ! { dg-error "Character dummy argument 'x2' at .1. must be of constant length of one or assumed length, unless it has assumed shape or assumed rank, as procedure 's2' has the BIND\\(C\\) attribute" }
  character(len=2) :: x2
end

subroutine s3 (xn, n) bind(C) ! { dg-error "Character dummy argument 'xn' at .1. must be of constant length of one or assumed length, unless it has assumed shape or assumed rank, as procedure 's3' has the BIND\\(C\\) attribute" }
  integer :: n
  character(len=n) :: xn
end

subroutine s4 (xstar) bind(C)
  character(len=*) :: xstar
end

! Assumed-shape array, nonallocatable/nonpointer

subroutine as1 (x1) bind(C)
  character(len=1) :: x1(:)
end

subroutine as2 (x2) bind(C)
  character(len=2) :: x2(:,:)
end

subroutine as3 (xn, n) bind(C)
  integer :: n
  character(len=n) :: xn(:,:,:)
end

subroutine as4 (xstar) bind(C)
  character(len=*) :: xstar(:,:,:,:)
end

! Assumed-rank array, nonallocatable/nonpointer

subroutine ar1 (x1) bind(C)
  character(len=1) :: x1(..)
end

subroutine ar2 (x2) bind(C)
  character(len=2) :: x2(..)
end

subroutine ar3 (xn, n) bind(C)
  integer :: n
  character(len=n) :: xn(..)
end

subroutine ar4 (xstar) bind(C)
  character(len=*) :: xstar(..)
end

! Assumed-size array, nonallocatable/nonpointer

subroutine az1 (x1) bind(C)
  character(len=1) :: x1(*)
end

subroutine az2 (x2) bind(C) ! { dg-error "Character dummy argument 'x2' at .1. must be of constant length of one or assumed length, unless it has assumed shape or assumed rank, as procedure 'az2' has the BIND\\(C\\) attribute" }
  character(len=2) :: x2(*)
end

subroutine az3 (xn, n) bind(C) ! { dg-error "Character dummy argument 'xn' at .1. must be of constant length of one or assumed length, unless it has assumed shape or assumed rank, as procedure 'az3' has the BIND\\(C\\) attribute" }
  integer :: n
  character(len=n) :: xn(*)
end

subroutine az4 (xstar) bind(C)
  character(len=*) :: xstar(*)
end

! Explicit-size array, nonallocatable/nonpointer

subroutine ae1 (x1) bind(C)
  character(len=1) :: x1(5)
end

subroutine ae2 (x2) bind(C) ! { dg-error "Character dummy argument 'x2' at .1. must be of constant length of one or assumed length, unless it has assumed shape or assumed rank, as procedure 'ae2' has the BIND\\(C\\) attribute" }
  character(len=2) :: x2(7)
end

subroutine ae3 (xn, n) bind(C) ! { dg-error "Character dummy argument 'xn' at .1. must be of constant length of one or assumed length, unless it has assumed shape or assumed rank, as procedure 'ae3' has the BIND\\(C\\) attribute" }
  integer :: n
  character(len=n) :: xn(9)
end

subroutine ae4 (xstar) bind(C)
  character(len=*) :: xstar(3)
end

! ALLOCATABLE
! Scalar, allocatable

subroutine s1a (x1) bind(C) ! { dg-error "Allocatable character dummy argument 'x1' at .1. must have deferred length as procedure 's1a' is BIND\\(C\\)" }
  character(len=1), allocatable :: x1
end

subroutine s2a (x2) bind(C) ! { dg-error "Allocatable character dummy argument 'x2' at .1. must have deferred length as procedure 's2a' is BIND\\(C\\)" }
  character(len=2), allocatable :: x2
end

subroutine s3a (xn, n) bind(C) ! { dg-error "Allocatable character dummy argument 'xn' at .1. must have deferred length as procedure 's3a' is BIND\\(C\\)" }
  integer :: n
  character(len=n), allocatable :: xn
end

subroutine s4a (xstar) bind(C) ! { dg-error "Allocatable character dummy argument 'xstar' at .1. must have deferred length as procedure 's4a' is BIND\\(C\\)" }
  character(len=*), allocatable :: xstar
end

subroutine s5a (xcolon) bind(C)
  character(len=:), allocatable :: xcolon
end

! Assumed-shape array, allocatable

subroutine a1a (x1) bind(C) ! { dg-error "Allocatable character dummy argument 'x1' at .1. must have deferred length as procedure 'a1a' is BIND\\(C\\)" }
  character(len=1), allocatable :: x1(:)
end

subroutine a2a (x2) bind(C) ! { dg-error "Allocatable character dummy argument 'x2' at .1. must have deferred length as procedure 'a2a' is BIND\\(C\\)" }
  character(len=2), allocatable :: x2(:,:)
end

subroutine a3a (xn, n) bind(C) ! { dg-error "Allocatable character dummy argument 'xn' at .1. must have deferred length as procedure 'a3a' is BIND\\(C\\)" }
  integer :: n
  character(len=n), allocatable :: xn(:,:,:)
end

subroutine a4a (xstar) bind(C) ! { dg-error "Allocatable character dummy argument 'xstar' at .1. must have deferred length as procedure 'a4a' is BIND\\(C\\)" }
  character(len=*), allocatable :: xstar(:,:,:,:)
end

subroutine a5a (xcolon) bind(C)
  character(len=:), allocatable :: xcolon(:)
end

! Assumed-rank array, allocatable

subroutine a1ar (x1) bind(C) ! { dg-error "Allocatable character dummy argument 'x1' at .1. must have deferred length as procedure 'a1ar' is BIND\\(C\\)" }
  character(len=1), allocatable :: x1(..)
end

subroutine a2ar (x2) bind(C) ! { dg-error "Allocatable character dummy argument 'x2' at .1. must have deferred length as procedure 'a2ar' is BIND\\(C\\)" }
  character(len=2), allocatable :: x2(..)
end

subroutine a3ar (xn, n) bind(C) ! { dg-error "Allocatable character dummy argument 'xn' at .1. must have deferred length as procedure 'a3ar' is BIND\\(C\\)" }
  integer :: n
  character(len=n), allocatable :: xn(..)
end

subroutine a4ar (xstar) bind(C) ! { dg-error "Allocatable character dummy argument 'xstar' at .1. must have deferred length as procedure 'a4ar' is BIND\\(C\\)" }
  character(len=*), allocatable :: xstar(..)
end

subroutine a5ar (xcolon) bind(C)
  character(len=:), allocatable :: xcolon(..)
end

! POINTER
! Scalar, pointer

subroutine s1p (x1) bind(C) ! { dg-error "Pointer character dummy argument 'x1' at .1. must have deferred length as procedure 's1p' is BIND\\(C\\)" }
  character(len=1), pointer :: x1
end

subroutine s2p (x2) bind(C) ! { dg-error "Pointer character dummy argument 'x2' at .1. must have deferred length as procedure 's2p' is BIND\\(C\\)" }
  character(len=2), pointer :: x2
end

subroutine s3p (xn, n) bind(C) ! { dg-error "Pointer character dummy argument 'xn' at .1. must have deferred length as procedure 's3p' is BIND\\(C\\)" }
  integer :: n
  character(len=n), pointer :: xn
end

subroutine s4p (xstar) bind(C) ! { dg-error "Pointer character dummy argument 'xstar' at .1. must have deferred length as procedure 's4p' is BIND\\(C\\)" }
  character(len=*), pointer :: xstar
end

subroutine s5p (xcolon) bind(C)
  character(len=:), pointer :: xcolon
end

! Assumed-shape array, pointer

subroutine a1p (x1) bind(C) ! { dg-error "Pointer character dummy argument 'x1' at .1. must have deferred length as procedure 'a1p' is BIND\\(C\\)" }
  character(len=1), pointer :: x1(:)
end

subroutine a2p (x2) bind(C) ! { dg-error "Pointer character dummy argument 'x2' at .1. must have deferred length as procedure 'a2p' is BIND\\(C\\)" }
  character(len=2), pointer :: x2(:,:)
end

subroutine a3p (xn, n) bind(C) ! { dg-error "Pointer character dummy argument 'xn' at .1. must have deferred length as procedure 'a3p' is BIND\\(C\\)" }
  integer :: n
  character(len=n), pointer :: xn(:,:,:)
end

subroutine a4p (xstar) bind(C) ! { dg-error "Pointer character dummy argument 'xstar' at .1. must have deferred length as procedure 'a4p' is BIND\\(C\\)" }
  character(len=*), pointer :: xstar(:,:,:,:)
end

subroutine a5p (xcolon) bind(C)
  character(len=:), pointer :: xcolon(:)
end

! Assumed-rank array, pointer

subroutine a1pr (x1) bind(C)  ! { dg-error "Pointer character dummy argument 'x1' at .1. must have deferred length as procedure 'a1pr' is BIND\\(C\\)" }
  character(len=1), pointer :: x1(..)
end

subroutine a2pr (x2) bind(C) ! { dg-error "Pointer character dummy argument 'x2' at .1. must have deferred length as procedure 'a2pr' is BIND\\(C\\)" }
  character(len=2), pointer :: x2(..)
end

subroutine a3pr (xn, n) bind(C) ! { dg-error "Pointer character dummy argument 'xn' at .1. must have deferred length as procedure 'a3pr' is BIND\\(C\\)" }
  integer :: n
  character(len=n), pointer :: xn(..)
end

subroutine a4pr (xstar) bind(C) ! { dg-error "Pointer character dummy argument 'xstar' at .1. must have deferred length as procedure 'a4pr' is BIND\\(C\\)" }
  character(len=*), pointer :: xstar(..)
end

subroutine a5pr (xcolon) bind(C)
  character(len=:), pointer :: xcolon(..)
end
