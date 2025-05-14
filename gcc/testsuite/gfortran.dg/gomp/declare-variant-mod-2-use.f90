! { dg-do compile  { target skip-all-targets } }
! used by declare-variant-mod-2.f90

! Check that module-file handling works for declare_variant
! and its match/adjust_args/append_args clauses
!
! PR fortran/115271

! THIS FILE PROCUEDES ERROR - SEE declare-variant-mod-2.f90 for dg-error lines

module m_test1
  use m1, only: my_m1_f => m1_f, my_m1_g => m1_g
  !$omp declare variant(my_m1_g : my_m1_f) match(construct={dispatch}) adjust_args(need_device_ptr: 1, 3) adjust_args(nothing: 1)
end

subroutine test1  ! See PR fortran/119288 - related to the following 'adjust_args' diagnostic
  use m_test1  ! { dg-error "'x' at .1. is specified more than once" }
  use iso_c_binding, only: c_ptr
  implicit none (type, external)
  type(c_ptr) :: a1,b1,c1
  integer :: i
  !$omp dispatch
    i = my_m1_g(a1,b1,c1)
end

subroutine test2
  use m2
  implicit none (type, external)
  integer :: i, t2_a1, t2_a2, t2_a3, t2_a4

  call m2_g(t2_a1)

  !$omp dispatch
    call m2_g(t2_a2)

  !$omp parallel if(.false.)
    !$omp dispatch
      call m2_g(t2_a3)
  !$omp end parallel

  !$omp do
  do i = 1, 1
    !$omp dispatch
      call m2_g(t2_a4)
  end do

end
