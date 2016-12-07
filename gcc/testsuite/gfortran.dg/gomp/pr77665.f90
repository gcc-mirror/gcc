! PR fortran/77665
! { dg-do compile }
! { dg-additional-options "-O2" }

program pr77665
  type t
    integer :: a = 0
  end type
  type(t) :: x
  integer :: i
  !$omp declare reduction (+:t: omp_out%a = omp_out%a + omp_in%a)
  !$omp simd reduction(+:x)
  do i = 1, 8
    if (abs(i) < 5) call abort
    x%a = x%a + 1
  end do
  print *, x%a
end
