! { dg-do  run }
! { dg-options "-O -fdump-tree-optimized -finline-matmul-limit=1000" }
module x
  implicit none
contains
  function afunc()
    real, dimension(3,3) :: afunc
    afunc = reshape([ 2., 3., 5., 7., 11., 13., 17., 19., 23. ], [3,3])
  end function afunc
  
  function bfunc()
    real, dimension(3,3) :: bfunc
    bfunc = reshape([29., 31., 37., 41., 43., 47., 53., 59., 61., 67.],[3,3])
  end function bfunc
end module x

program main
  use x
  implicit none
  real, dimension(3,3), parameter :: &
       & aval = reshape([ 2., 3., 5., 7., 11., 13., 17., 19., 23. ], [3,3]), &
       & bval = reshape([29., 31., 37., 41., 43., 47., 53., 59., 61., 67.],[3,3])
  integer, dimension(3) :: ind
  real, dimension(3,3) :: a, b,c,d, ri
  data ri /120430., 187861., 151737., 161022., 251139., 202847., 212566., 331537., 267781./
  data d/904., 1131., 1399., 1182., 1489., 1845., 1556., 1967., 2435. /
  a = aval
  b = bval
  c = matmul(a,b)
  a = matmul(a,b)
  if (any(a-c /= 0)) STOP 1
  a = aval
  b = bval
  b = matmul(a,b)
  if (any(b-c /= 0)) STOP 2
  b = bval
  a = matmul(aval, b)
  if (any(a-c /= 0)) STOP 3
  ind = [1, 3, 2]
  c = matmul(a(ind,:),b)
  if (any(c-ri /= 0)) STOP 4
  c = matmul(afunc(),b)
  if (any(c-d /= 0)) STOP 5
  a = afunc()
  c = matmul(a, bfunc())
  if (any(c-d /= 0)) STOP 6
end program main
! { dg-final { scan-tree-dump-times "matmul_r4" 2 "optimized" } }
