! { dg-do compile }
! { dg-options "-std=f2003" }
! PR fortran/77652 - Invalid rank error in ASSOCIATED when rank is remapped
! Contributed by Paul Thomas

subroutine s
  real, dimension(100),  target  :: array
  real, dimension(:,:),  pointer :: matrix
  real, dimension(20,5), target  :: array2
  real, dimension(:),    pointer :: matrix2
  real,                  pointer :: scalar, scalar2
  scalar => scalar2
  print *, associated (scalar, scalar2)

  matrix(1:20,1:5) => array            ! F2003+
! matrix2(1:100)   => array2           ! F2008+
  print *, associated (matrix, array ) ! Technically legal F2003
  print *, associated (matrix2,array2) ! { dg-error "is not rank 1" }

  ! There exists no related valid pointer assignment for these cases:
  print *, associated (scalar,matrix2) ! { dg-error "must be of rank 0" }
  print *, associated (matrix2,scalar) ! { dg-error "must be of rank 1" }
end
