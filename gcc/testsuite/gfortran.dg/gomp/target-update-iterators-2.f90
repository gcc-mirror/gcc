! { dg-do compile }
! { dg-options "-fopenmp -fdump-tree-gimple" }

program test
  implicit none

  integer, parameter :: DIM1 = 100

  type :: array_ptr
    integer, pointer :: ptr(:)
  end type

  type (array_ptr) :: x(DIM1), y(DIM1), z(DIM1)

  !$omp target update to(iterator(i=1:10): x) ! { dg-warning "iterator variable .i. not used in clause expression" }
  !$omp target update from(iterator(i2=1:10, j2=1:20): x(i2)) ! { dg-warning "iterator variable .j2. not used in clause expression" }
  !$omp target update to(iterator(i3=1:10, j3=1:20, k3=1:30): x(i3+j3), y(j3+k3), z(k3+i3))
  ! { dg-warning "iterator variable .i3. not used in clause expression" "" { target *-*-* } .-1 }
  ! { dg-warning "iterator variable .j3. not used in clause expression" "" { target *-*-* } .-2 }
  ! { dg-warning "iterator variable .k3. not used in clause expression" "" { target *-*-* } .-3 }
end program

! { dg-final { scan-tree-dump-times "update to\\\(x " 1 "gimple" } }
! { dg-final { scan-tree-dump-times "update from\\\(iterator\\\(integer\\\(kind=4\\\) i2=1:10:1, loop_label=" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "to\\\(iterator\\\(integer\\\(kind=4\\\) j3=1:20:1, integer\\\(kind=4\\\) i3=1:10:1, loop_label=" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "to\\\(iterator\\\(integer\\\(kind=4\\\) k3=1:30:1, integer\\\(kind=4\\\) j3=1:20:1, loop_label=" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "to\\\(iterator\\\(integer\\\(kind=4\\\) k3=1:30:1, integer\\\(kind=4\\\) i3=1:10:1, loop_label=" 1 "gimple" } }
 