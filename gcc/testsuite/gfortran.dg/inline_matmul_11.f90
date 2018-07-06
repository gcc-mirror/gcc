! { dg-do  run }
! { dg-additional-options "-ffrontend-optimize -fdump-tree-optimized" }
! PR fortran/66176 - inline conjg for matml.
program main
  complex, dimension(3,2) :: a
  complex, dimension(2,4) :: b, b2
  complex, dimension(3,4) :: c,c2
  complex, dimension(3,4) :: res1, res2, res3

  data a/(2.,-3.),(-5.,-7.),(11.,-13.),(-17.,-19.),(23.,-29.),(-31.,-37.) /
  data b/(41.,-43.),(-47.,-53.),(59.,-61.),(-67.,-71.),(73.,-79.),&
       & (-83.,-89.),(97.,-101.), (-103.,-107.)/

  data res1 /  (-255.,1585.),(-3124.,72.),(-612.,2376.),(-275.,2181.), &
       & (-4322.,202.),(-694.,3242.),(-371.,2713.),( -5408.,244.),(-944.,4012.),&
       & (-391.,3283.),(-6664.,352.),(-1012.,4756.)/

  data res2 / (2017.,-45.),(552.,2080.),(4428.,36.),(2789.,11.),(650.,2858.),&
       & (6146.,182.),(3485.,3.),(860.,3548.),(7696.,232.),(4281.,49.),&
       & (956.,4264.),(9532.,344.)/

  c = matmul(a,b)
  if (any(res1 /= c)) STOP 1
  b2 = conjg(b)
  c = matmul(a,conjg(b2))
  if (any(res1 /= c)) STOP 2
  c = matmul(a,conjg(b))
  if (any(res2 /= c)) STOP 3
  c = matmul(conjg(a), b)
  if (any(conjg(c) /= res2)) STOP 4
end program main
! { dg-final { scan-tree-dump-times "_gfortran_matmul" 0 "optimized" } }
