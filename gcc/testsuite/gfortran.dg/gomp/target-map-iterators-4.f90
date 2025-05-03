! { dg-do compile }
! { dg-options "-fopenmp -fdump-tree-gimple" }

module m
  !$omp declare target (baz)
  interface
    subroutine baz (x, p)
      integer, intent(in) :: x
      integer, pointer :: p(:)
    end subroutine
    integer function bar (x, i)
      integer :: x, i
    end function
  end interface
contains
  subroutine foo (x, p)
    integer :: x
    integer, pointer :: p(:)

    !$omp target map (iterator (i=1:4), to: p(bar (x, i)))
      ! FIXME: These warnings are due to implicit clauses generated that do
      ! not use the iterator variable i.
      ! { dg-warning "iterator variable .i. not used in clause expression" "" { target *-*-* } .-3 }
      call baz (x, p)
    !$omp end target
  end subroutine
end module

! { dg-final { scan-tree-dump "firstprivate\\\(x\\\)" "gimple" } }
! { dg-final { scan-tree-dump-times "bar \\\(x, &" 2 "gimple" } }
! { dg-final { scan-tree-dump "map\\\(iterator\\\(integer\\\(kind=4\\\) i=1:4:1, loop_label=" "gimple" } }
