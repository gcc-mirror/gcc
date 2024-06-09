! { dg-do compile }
!
! This failed to compile the declare variant directive due to the C_PTR 
! arguments to foo being recognised as INTEGER(8)

program adjust_args
  use iso_c_binding, only: c_loc
  implicit none

  integer, parameter :: N = 1024
  real, allocatable, target :: av(:), bv(:), cv(:)

  call foo(c_loc(bv), c_loc(av), N)

  !$omp target data map(to: av(:N)) map(from: cv(:N))
      !$omp parallel
      call foo(c_loc(cv), c_loc(av), N)
      !$omp end parallel
  !$omp end target data
    
contains
  subroutine foo_variant(c_d_bv, c_d_av, n)
    use iso_c_binding, only: c_ptr, c_f_pointer
    type(c_ptr), intent(in) :: c_d_bv, c_d_av
    integer, intent(in) :: n
    real, pointer :: f_d_bv(:)
    real, pointer :: f_d_av(:)
    integer :: i
  
    call c_f_pointer(c_d_bv, f_d_bv, [n])
    call c_f_pointer(c_d_av, f_d_av, [n])
    !$omp target teams loop is_device_ptr(f_d_bv, f_d_av)
    do i = 1, n
      f_d_bv(i) = f_d_av(i) * i
    end do
  end subroutine
  
  
  subroutine foo(c_bv, c_av, n)
    use iso_c_binding, only: c_ptr, c_f_pointer
    type(c_ptr), intent(in) :: c_bv, c_av
    integer, intent(in) :: n
    real, pointer :: f_bv(:)
    real, pointer :: f_av(:)
    integer :: i
    !$omp declare variant(foo_variant)          &
    !$omp         match(construct={parallel})       
  
    call c_f_pointer(c_bv, f_bv, [n])
    call c_f_pointer(c_av, f_av, [n])
    !$omp parallel loop
    do i = 1, n
      f_bv(i) = f_av(i) * i
    end do
  end subroutine
end program
