! { dg-do run }
module m2
  type :: base(k, n)
    integer, kind :: k = 4
    integer, len :: n = 1
    real(k) :: v(n)
  end type
end module
program i2
  use m2
  class(*), allocatable :: u
  allocate (base(4, 3) :: u)
!  allocate (u, source = base(4, 3)([1,2,3])) ! Was OK
  select type (u)
  type is (base(4, *))
    if (u%n /= 3) stop 1
    if (size (u%v) /= 3) stop 2
  class default
    stop 3
  end select
end program
