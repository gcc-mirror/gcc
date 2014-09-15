! { dg-do compile }
! { dg-options "-fcoarray=single" }

! To be used by coarray_29_2.f90
! PR fortran/55272

module co_sum_module
  implicit none
contains
  subroutine co_sum(scalar)
    integer scalar[*]
  end subroutine
end module
