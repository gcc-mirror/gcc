! { dg-do compile }

module mymod
  implicit none (type, external)
  integer, target :: var(0:5) = [0,1,2,3,4,5]
contains
  function foo (i)
    integer :: i
    integer, pointer :: foo
    foo => var(mod(i, 6))
  end
end module mymod

program main
  use mymod
  implicit none

  type t
    integer :: x(0:64)
    integer :: y
  end type t
  type(t) :: dep2(0:64)
  integer :: dep1(0:64)

  integer arr(0:63)
  !$omp parallel
  !$omp master
  block
    integer :: i
    do i = 0, 63
      ! NB: Revoking foo (pointer returning function) as in 'foo(i)' is a variable in the Fortran sense
      !$omp task depend (iterator (j=i:i+1) , out : foo (j)) ! { dg-error "is not a variable" }
        arr(i) = i
      !!$omp end task
    !$omp task depend(iterator(i=1:5), source )  ! { dg-error "ITERATOR may not be combined with SOURCE" }
  !!$omp end task
  !$omp task affinity (iterator(i=1:5): a) depend(iterator(i=1:5), sink : x) ! { dg-error "SINK at .1. not permitted as dependence-type for this directive" }
  !!$omp end task

    end do
  end block
  !$omp end master
  !$omp end parallel
end
