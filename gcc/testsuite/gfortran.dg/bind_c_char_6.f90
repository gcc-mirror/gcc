! { dg-do compile }
! { dg-additional-options "-std=f2003 -fimplicit-none" }

! F2003 only permits length=1 character dummy args

! Scalar, nonallocatable/nonpointer

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

subroutine s4 (xstar) bind(C) ! { dg-error "Fortran 2018: Assumed-length character dummy argument 'xstar' at .1. of procedure 's4' with BIND\\(C\\) attribute" }
  character(len=*) :: xstar
end

! Assumed-shape array, nonallocatable/nonpointer

subroutine as1 (x1) bind(C)  ! { dg-error "Fortran 2018: Assumed-shape array 'x1' at .1. as dummy argument to the BIND\\(C\\) procedure 'as1' at .2." }
  character(len=1) :: x1(:)
end

subroutine as2 (x2) bind(C) ! { dg-error "Fortran 2018: Assumed-shape array 'x2' at .1. as dummy argument to the BIND\\(C\\) procedure 'as2' at .2." }
  character(len=2) :: x2(:,:)
end

subroutine as3 (xn, n) bind(C) ! { dg-error "Fortran 2018: Assumed-shape array 'xn' at .1. as dummy argument to the BIND\\(C\\) procedure 'as3' at .2." }
  integer :: n
  character(len=n) :: xn(:,:,:)
end

subroutine as4 (xstar) bind(C) ! { dg-error "Fortran 2018: Assumed-length character dummy argument 'xstar' at .1. of procedure 'as4' with BIND\\(C\\) attribute"  }
                               ! { dg-error "Fortran 2018: Assumed-shape array 'xstar' at .1. as dummy argument to the BIND\\(C\\) procedure 'as4' at .2." "" { target *-*-* } .-1 }
  character(len=*) :: xstar(:,:,:,:)
end

! Assumed-rank array, nonallocatable/nonpointer

subroutine ar1 (x1) bind(C) ! { dg-error "Symbol 'x1' at .1. has no IMPLICIT type" }
  character(len=1) :: x1(..) ! { dg-error "Fortran 2018: Assumed-rank array at .1." }
end

subroutine ar2 (x2) bind(C) ! { dg-error "Symbol 'x2' at .1. has no IMPLICIT type" }
  character(len=2) :: x2(..) ! { dg-error "Fortran 2018: Assumed-rank array at .1." }
end

subroutine ar3 (xn, n) bind(C) ! { dg-error "Symbol 'xn' at .1. has no IMPLICIT type" }
  integer :: n
  character(len=n) :: xn(..) ! { dg-error "Fortran 2018: Assumed-rank array at .1." }
end

subroutine ar4 (xstar) bind(C) ! { dg-error "Symbol 'xstar' at .1. has no IMPLICIT type" }
  character(len=*) :: xstar(..) ! { dg-error "Fortran 2018: Assumed-rank array at .1." }
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

subroutine az4 (xstar) bind(C) ! { dg-error "Fortran 2018: Assumed-length character dummy argument 'xstar' at .1. of procedure 'az4' with BIND\\(C\\) attribute" }
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

subroutine ae4 (xstar) bind(C) ! { dg-error "Fortran 2018: Assumed-length character dummy argument 'xstar' at .1. of procedure 'ae4' with BIND\\(C\\) attribute" }
  character(len=*) :: xstar(3)
end

! ALLOCATABLE
! Scalar, allocatable

subroutine s1a (x1) bind(C) ! { dg-error "Allocatable character dummy argument 'x1' at .1. must have deferred length as procedure 's1a' is BIND\\(C\\)" }
                            ! { dg-error "Fortran 2018: Variable 'x1' at .1. with ALLOCATABLE attribute in procedure 's1a' with BIND\\(C\\)" "" { target *-*-* } .-1 }
  character(len=1), allocatable :: x1
end

subroutine s2a (x2) bind(C) ! { dg-error "Allocatable character dummy argument 'x2' at .1. must have deferred length as procedure 's2a' is BIND\\(C\\)" }
                            ! { dg-error "Fortran 2018: Variable 'x2' at .1. with ALLOCATABLE attribute in procedure 's2a' with BIND\\(C\\)" "" { target *-*-* } .-1 }
  character(len=2), allocatable :: x2
end

subroutine s3a (xn, n) bind(C) ! { dg-error "Allocatable character dummy argument 'xn' at .1. must have deferred length as procedure 's3a' is BIND\\(C\\)" }
                               ! { dg-error "Fortran 2018: Variable 'xn' at .1. with ALLOCATABLE attribute in procedure 's3a' with BIND\\(C\\)" "" { target *-*-* } .-1 }
  integer :: n
  character(len=n), allocatable :: xn
end

subroutine s4a (xstar) bind(C) ! { dg-error "Allocatable character dummy argument 'xstar' at .1. must have deferred length as procedure 's4a' is BIND\\(C\\)" }
                               ! { dg-error "Fortran 2018: Variable 'xstar' at .1. with ALLOCATABLE attribute in procedure 's4a' with BIND\\(C\\)" "" { target *-*-* } .-1 }
  character(len=*), allocatable :: xstar
end

subroutine s5a (xcolon) bind(C) ! { dg-error "Fortran 2018: Deferred-length character dummy argument 'xcolon' at .1. of procedure 's5a' with BIND\\(C\\) attribute" }
                                ! { dg-error "Fortran 2018: Variable 'xcolon' at .1. with ALLOCATABLE attribute in procedure 's5a' with BIND\\(C\\)" "" { target *-*-* } .-1 }
  character(len=:), allocatable :: xcolon
end

! Assumed-shape array, allocatable

subroutine a1a (x1) bind(C) ! { dg-error "Allocatable character dummy argument 'x1' at .1. must have deferred length as procedure 'a1a' is BIND\\(C\\)" }
                            ! { dg-error "Fortran 2018: Variable 'x1' at .1. with ALLOCATABLE attribute in procedure 'a1a' with BIND\\(C\\)" "" { target *-*-* } .-1 }
  character(len=1), allocatable :: x1(:)
end

subroutine a2a (x2) bind(C) ! { dg-error "Allocatable character dummy argument 'x2' at .1. must have deferred length as procedure 'a2a' is BIND\\(C\\)" }
                            ! { dg-error "Fortran 2018: Variable 'x2' at .1. with ALLOCATABLE attribute in procedure 'a2a' with BIND\\(C\\)" "" { target *-*-* } .-1 }
  character(len=2), allocatable :: x2(:,:)
end

subroutine a3a (xn, n) bind(C) ! { dg-error "Allocatable character dummy argument 'xn' at .1. must have deferred length as procedure 'a3a' is BIND\\(C\\)" }
                               ! { dg-error "Fortran 2018: Variable 'xn' at .1. with ALLOCATABLE attribute in procedure 'a3a' with BIND\\(C\\)" "" { target *-*-* } .-1 }
  integer :: n
  character(len=n), allocatable :: xn(:,:,:)
end

subroutine a4a (xstar) bind(C) ! { dg-error "Allocatable character dummy argument 'xstar' at .1. must have deferred length as procedure 'a4a' is BIND\\(C\\)" }
                               ! { dg-error "Fortran 2018: Variable 'xstar' at .1. with ALLOCATABLE attribute in procedure 'a4a' with BIND\\(C\\)" "" { target *-*-* } .-1 }
  character(len=*), allocatable :: xstar(:,:,:,:)
end

subroutine a5a (xcolon) bind(C) ! { dg-error "Fortran 2018: Deferred-length character dummy argument 'xcolon' at .1. of procedure 'a5a' with BIND\\(C\\) attribute" }
                                ! { dg-error "Fortran 2018: Variable 'xcolon' at .1. with ALLOCATABLE attribute in procedure 'a5a' with BIND\\(C\\)" "" { target *-*-* } .-1 }
  character(len=:), allocatable :: xcolon(:)
end

! Assumed-rank array, allocatable

subroutine a1ar (x1) bind(C) ! { dg-error "Symbol 'x1' at .1. has no IMPLICIT type" }
  character(len=1), allocatable :: x1(..) ! { dg-error "Fortran 2018: Assumed-rank array at .1." }
end

subroutine a2ar (x2) bind(C) ! { dg-error "Symbol 'x2' at .1. has no IMPLICIT type" }
  character(len=2), allocatable :: x2(..) ! { dg-error "Fortran 2018: Assumed-rank array at .1." }
end

subroutine a3ar (xn, n) bind(C) ! { dg-error "Symbol 'xn' at .1. has no IMPLICIT type" }
  integer :: n
  character(len=n), allocatable :: xn(..) ! { dg-error "Fortran 2018: Assumed-rank array at .1." }
end

subroutine a4ar (xstar) bind(C) ! { dg-error "Symbol 'xstar' at .1. has no IMPLICIT type" }
  character(len=*), allocatable :: xstar(..) ! { dg-error "Fortran 2018: Assumed-rank array at .1." }
end

subroutine a5ar (xcolon) bind(C) ! { dg-error "Symbol 'xcolon' at .1. has no IMPLICIT type" }
  character(len=:), allocatable :: xcolon(..) ! { dg-error "Fortran 2018: Assumed-rank array at .1." }
end

! POINTER
! Scalar, pointer

subroutine s1p (x1) bind(C) ! { dg-error "Pointer character dummy argument 'x1' at .1. must have deferred length as procedure 's1p' is BIND\\(C\\)" }
                            ! { dg-error "Fortran 2018: Variable 'x1' at .1. with POINTER attribute in procedure 's1p' with BIND\\(C\\)" "" { target *-*-* } .-1 }
  character(len=1), pointer :: x1
end

subroutine s2p (x2) bind(C) ! { dg-error "Pointer character dummy argument 'x2' at .1. must have deferred length as procedure 's2p' is BIND\\(C\\)" }
                            ! { dg-error "Fortran 2018: Variable 'x2' at .1. with POINTER attribute in procedure 's2p' with BIND\\(C\\)" "" { target *-*-* } .-1 }
  character(len=2), pointer :: x2
end

subroutine s3p (xn, n) bind(C) ! { dg-error "Pointer character dummy argument 'xn' at .1. must have deferred length as procedure 's3p' is BIND\\(C\\)" }
                               ! { dg-error "Fortran 2018: Variable 'xn' at .1. with POINTER attribute in procedure 's3p' with BIND\\(C\\)" "" { target *-*-* } .-1 }
  integer :: n
  character(len=n), pointer :: xn
end

subroutine s4p (xstar) bind(C) ! { dg-error "Pointer character dummy argument 'xstar' at .1. must have deferred length as procedure 's4p' is BIND\\(C\\)" }
                               ! { dg-error "Fortran 2018: Variable 'xstar' at .1. with POINTER attribute in procedure 's4p' with BIND\\(C\\)" "" { target *-*-* } .-1 }
  character(len=*), pointer :: xstar
end

subroutine s5p (xcolon) bind(C) ! { dg-error "Fortran 2018: Deferred-length character dummy argument 'xcolon' at .1. of procedure 's5p' with BIND\\(C\\) attribute" }
                            ! { dg-error "Fortran 2018: Variable 'xcolon' at .1. with POINTER attribute in procedure 's5p' with BIND\\(C\\)" "" { target *-*-* } .-1 }
  character(len=:), pointer :: xcolon
end

! Assumed-shape array, pointer

subroutine a1p (x1) bind(C) ! { dg-error "Pointer character dummy argument 'x1' at .1. must have deferred length as procedure 'a1p' is BIND\\(C\\)" }
                            ! { dg-error "Fortran 2018: Variable 'x1' at .1. with POINTER attribute in procedure 'a1p' with BIND\\(C\\)" "" { target *-*-* } .-1 }
  character(len=1), pointer :: x1(:)
end

subroutine a2p (x2) bind(C) ! { dg-error "Pointer character dummy argument 'x2' at .1. must have deferred length as procedure 'a2p' is BIND\\(C\\)" }
                            ! { dg-error "Fortran 2018: Variable 'x2' at .1. with POINTER attribute in procedure 'a2p' with BIND\\(C\\)" "" { target *-*-* } .-1 }
  character(len=2), pointer :: x2(:,:)
end

subroutine a3p (xn, n) bind(C) ! { dg-error "Pointer character dummy argument 'xn' at .1. must have deferred length as procedure 'a3p' is BIND\\(C\\)" }
                               ! { dg-error "Fortran 2018: Variable 'xn' at .1. with POINTER attribute in procedure 'a3p' with BIND\\(C\\)" "" { target *-*-* } .-1 }
  integer :: n
  character(len=n), pointer :: xn(:,:,:)
end

subroutine a4p (xstar) bind(C) ! { dg-error "Pointer character dummy argument 'xstar' at .1. must have deferred length as procedure 'a4p' is BIND\\(C\\)" }
                               ! { dg-error "Fortran 2018: Variable 'xstar' at .1. with POINTER attribute in procedure 'a4p' with BIND\\(C\\)" "" { target *-*-* } .-1 }
  character(len=*), pointer :: xstar(:,:,:,:)
end

subroutine a5p (xcolon) bind(C) ! { dg-error "Fortran 2018: Deferred-length character dummy argument 'xcolon' at .1. of procedure 'a5p' with BIND\\(C\\) attribute" }
                                ! { dg-error "Fortran 2018: Variable 'xcolon' at .1. with POINTER attribute in procedure 'a5p' with BIND\\(C\\)" "" { target *-*-* } .-1 }
  character(len=:), pointer :: xcolon(:)
end

! Assumed-rank array, pointer

subroutine a1pr (x1) bind(C) ! { dg-error "Symbol 'x1' at .1. has no IMPLICIT type" }
  character(len=1), pointer :: x1(..) ! { dg-error "Fortran 2018: Assumed-rank array at .1." }
end

subroutine a2pr (x2) bind(C) ! { dg-error "Symbol 'x2' at .1. has no IMPLICIT type" }
  character(len=2), pointer :: x2(..) ! { dg-error "Fortran 2018: Assumed-rank array at .1." }
end

subroutine a3pr (xn, n) bind(C) ! { dg-error "Symbol 'xn' at .1. has no IMPLICIT type" }
  integer :: n
  character(len=n), pointer :: xn(..) ! { dg-error "Fortran 2018: Assumed-rank array at .1." }
end

subroutine a4pr (xstar) bind(C) ! { dg-error "Symbol 'xstar' at .1. has no IMPLICIT type" }
  character(len=*), pointer :: xstar(..) ! { dg-error "Fortran 2018: Assumed-rank array at .1." }
end

subroutine a5pr (xcolon) bind(C) ! { dg-error "Symbol 'xcolon' at .1. has no IMPLICIT type" }
  character(len=:), pointer :: xcolon(..) ! { dg-error "Fortran 2018: Assumed-rank array at .1." }
end
