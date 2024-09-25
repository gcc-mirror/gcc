!{ dg-do run }

! Check that pr77518 is fixed.
! Based on code by Gerhard Steinmetz  <gerhard.steinmetz.fortran@t-online.de>

program coarray_sizeof_1
  type t
  end type
  type t2
    integer :: v = 42
  end type
  type t3
    type(t2) :: s
    integer :: n = 1
  end type

  class(t), allocatable :: z[:]
  class(t2), allocatable :: z2[:]
  class(t3), allocatable :: z3[:]

  if (sizeof(z) /= 0) stop 1
  if (sizeof(z2) /= sizeof(integer)) stop 2
  allocate(z3[*])
  if (sizeof(z3) /= sizeof(z2) + sizeof(integer)) stop 3
  if (sizeof(z3%s) /= sizeof(z2)) stop 4
end

