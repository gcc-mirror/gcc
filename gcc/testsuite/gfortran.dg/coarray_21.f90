! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! PR fortran/18918
!
! Before scalar coarrays weren't regarded as scalar in the ME.
!
module mod_reduction
  real :: g[*]
contains
  subroutine caf_reduce(x)
    real, intent(in) :: x
       g = x  ! << used to ICE
  end
end module

program test
  integer, parameter :: size = 4000
  type :: pct
    integer, allocatable :: data(:,:)
  end type
  type(pct) :: picture[*]
     allocate(picture%data(size, size))
end program test


! { dg-final { cleanup-modules "mod_reduction" } }
