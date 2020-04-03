! { dg-additional-options "-O0 -Wno-analyzer-possible-null-dereference -Wno-analyzer-null-dereference -Wno-analyzer-malloc-leak" }

program cb
  implicit none
  type :: jn
     real, allocatable :: ie
     character(len = :), allocatable :: e5
  end type jn
  real, parameter :: gm = 5.0

  block
    type(jn) :: r2

    r2 = jn (gm, "")
    call vz (r2%ie, gm)
  end block
contains
  subroutine vz (arg1, arg2)
    real :: arg1, arg2
    if (arg1 .ne. arg2) STOP 1
  end subroutine vz
end program cb
