! { dg-do compile }

module mymod
  implicit none (type, external)
  integer, target :: var(0:5) = [0,1,2,3,4,5]
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
      !$omp task depend (iterator (j=i:i+1) , out : dep1 (j))
        arr(i) = i
      !$omp end task
      !$omp task depend (iterator (j=i:i+1) , out : dep2 (j))
        arr(i) = i
      !$omp end task
      !$omp task depend (iterator (j=i:i+1) , out : dep2 (j)%y)
        arr(i) = i
      !$omp end task
      !$omp task depend (iterator (j=i:i+1) , out : dep2 (j)%x(j))
        arr(i) = i
      !$omp end task
      !$omp task depend (out : dep2 (:4))
        arr(i) = i
      !$omp end task
      !$omp taskwait depend(out: dep1(1))
    end do
  end block
  !$omp end master
  !$omp end parallel
end
