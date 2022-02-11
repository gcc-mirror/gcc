! { dg-do run }
! PR libfortran/103634 - Runtime crash with PACK on zero-sized arrays
! Exercise PACK intrinsic for cases when it calls pack_internal

program p
  implicit none
  type t
     real :: r(24) = -99.
  end type
  type(t), allocatable :: new(:), old(:), vec(:)
  logical, allocatable :: mask(:)
  integer              :: n, m
! m = 1    ! works
  m = 0    ! failed with SIGSEGV in pack_internal
  do m = 0, 2
     print *, m
     allocate (old(m), mask(m), vec(m))
     if (m > 0) vec(m)% r(1) = 42
     mask(:) = .true.
     n = count (mask)
     allocate (new(n))

     mask(:) = .false.
     if (size (pack (old, mask)) /= 0) stop 1
     mask(:) = .true.
     if (size (pack (old, mask)) /= m) stop 2
     new(:) = pack (old, mask)              ! this used to segfault for m=0

     mask(:) = .false.
     if (size (pack (old, mask, vector=vec)) /= m) stop 3
     new(:) = t()
     new(:) = pack (old, mask, vector=vec)  ! this used to segfault for m=0
     if (m > 0) then
        if (     new( m  )% r(1) /=  42)  stop 4
        if (any (new(:m-1)% r(1) /= -99)) stop 5
     end if

     if (m > 0) mask(m) = .true.
     if (size (pack (old, mask, vector=vec)) /= m) stop 6
     new(:) = t()
     new(:) = pack (old, mask, vector=vec)  ! this used to segfault for m=0
     if (m > 0) then
        if (new(1)% r(1) /= -99) stop 7
     end if
     if (m > 1) then
        if (new(m)% r(1) /=  42) stop 8
     end if

     if (size (pack (old(:0), mask(:0), vector=vec)) /= m) stop 9
     new(:) = t()
     new(:) = pack (old(:0), mask(:0), vector=vec) ! did segfault for m=0
     if (m > 0) then
        if (new(m)% r(1) /= 42) stop 10
     end if
     deallocate (old, mask, new, vec)
  end do
end
