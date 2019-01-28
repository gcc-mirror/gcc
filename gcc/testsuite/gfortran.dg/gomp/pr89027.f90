! PR tree-optimization/89027
! { dg-do compile }
! { dg-additional-options "-O2 -fexceptions -fno-tree-dce" }

subroutine bar
  integer :: a, b
  a = 1
  b = 2
  call foo
contains
  subroutine foo
!$omp simd linear(a:2) linear(b:1)
    do a = 1, 20, 2
      b = b + 1
    end do
!$omp end simd
    if (a /= 21 .or. b /= 12) STOP 1
!$omp task depend(out : a)
    a = a + 1
!$omp end task
  end subroutine foo
end subroutine bar
