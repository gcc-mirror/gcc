! { dg-do run }
! { dg-options "-std=f2018" }
! PR fortran/77652 - Invalid rank error in ASSOCIATED when rank is remapped
! Contributed by Paul Thomas

program p
  real, dimension(100),  target  :: array
  real, dimension(:,:),  pointer :: matrix
  real, dimension(20,5), target  :: array2
  real, dimension(:),    pointer :: matrix2
  matrix(1:20,1:5) => array
  matrix2(1:100)   => array2
  !
  ! F2018:16.9.16, ASSOCIATED (POINTER [, TARGET])
  ! Case(v): If TARGET is present and is an array target, the result is
  ! true if and only if POINTER is associated with a target that has
  ! the same shape as TARGET, ...
  if (associated (matrix, array )) stop 1
  if (associated (matrix2,array2)) stop 2
  call check (matrix2, array2)
contains
  subroutine check (ptr, tgt)
    real, pointer :: ptr(..)
    real, target  :: tgt(:,:)
    if (associated (ptr, tgt)) stop 3
  end subroutine check
end
