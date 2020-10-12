! { dg-do compile }
! { dg-options "-O1 -Wall" }
!
! PR fortran/96312. The line with the call to 'matmul' gave the warning
! ‘tmp.dim[0].lbound’ is used uninitialized in this function
!
! Contributed by Thomas Koenig  <tkoenig@gcc.gnu.org>
!
module moda
contains
   PURE SUBROUTINE funca(arr, sz)
      REAL, ALLOCATABLE, DIMENSION(:, :), INTENT(OUT) :: arr
      integer, intent(in) :: sz
      allocate(arr(sz, sz))
      arr(:, :) = 0.
   END SUBROUTINE
end module

module modc
    use moda, only: funca
contains
   PURE SUBROUTINE funcb(oarr)
      REAL, DIMENSION(:), INTENT(OUT)    :: oarr
      REAL, ALLOCATABLE, DIMENSION(:, :) :: arr
      real, allocatable, dimension(:) :: tmp
      CALL funca(arr, ubound(oarr, 1))
      tmp = matmul(transpose(arr),oarr)
      oarr = tmp*1.
   END SUBROUTINE funcb
end module
