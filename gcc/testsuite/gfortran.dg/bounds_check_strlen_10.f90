! { dg-do compile }
! { dg-additional-options "-fcheck=bounds -fdump-tree-optimized" }
!
! PR fortran/53357 - bounds-check for character type-spec in ALLOCATE

program pr53357
  implicit none
  integer :: i, j
  i = 3
  j = 5
  block
    character(len=i), allocatable :: str1
    character(len=j), allocatable :: str2
    allocate (character(len=3) :: &
         str1, & ! runtime check optimized away
         str2  ) ! runtime check kept
  end block
end

! { dg-final { scan-tree-dump-times "_gfortran_runtime_error_at" 1 "optimized" } }
! { dg-final { scan-tree-dump-times "At line 16 of file" 1 "optimized" } }
