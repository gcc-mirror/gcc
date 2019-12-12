! { dg-do compile }
! { dg-options "-fopenmp -fdump-tree-original" }

! PR fortran/78260

integer function f1()
  implicit none

  f1 = 0

  !$omp task depend(inout:f1)
  !$omp end task

  !$omp task depend(inout:f1)
  !$omp end task
end function f1

integer function f2()
  implicit none
  dimension :: f2(1)

  f2(1) = 0

  !$omp task depend(inout:f2)
  !$omp end task

  !$omp task depend(inout:f2)
  !$omp end task
end function f2

integer function f3() result(res)
  implicit none
  dimension :: res(1)

  res(1) = 0

  !$omp task depend(inout:res)
  !$omp end task

  !$omp task depend(inout:res)
  !$omp end task
end function f3

integer function f4() result(res)
  implicit none
  allocatable :: res
  dimension :: res(:)

  res = [0]

  !$omp task depend(inout:res)
  !$omp end task

  !$omp task depend(inout:res)
  !$omp end task
end function f4

subroutine sub()
  implicit none
  integer, allocatable :: arr(:)

  arr = [3]

  !$omp task depend(inout:arr)
  !$omp end task

  !$omp task depend(inout:arr)
  !$omp end task
end subroutine sub

! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(inout:__result_f1\\)" 2 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(inout:\\*__result.0\\)" 4 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(inout:\\*\\(c_char \\*\\) __result->data\\)" 2 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(inout:\\*\\(c_char \\*\\) arr.data\\)" 2 "original" } }
