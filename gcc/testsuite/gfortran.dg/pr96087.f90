! { dg-do run }

module m
   interface
      module function f(a, n, b) result(z)
         integer, intent(in) :: n
         real :: z(n + 1)
         real :: a, b
      end
   end interface
contains
   module procedure f
      integer :: i
      do i = 1, size(z)
        z(i) = real(i)
      end do
   end procedure
end

! Comment 1
module n
   interface
      module subroutine g(n, z)
         integer, intent(in) :: n
         real :: z(n)
      end
   end interface
contains
   module procedure g
      z = 1
      if (int (sum (z)) /= n) stop 1
   end procedure
end

  use m
  use n
  real, allocatable :: r(:)
  integer :: i = 2
  r = f (1.0, i+1, 2.0)
  if (any (r .ne. [(real(i), i = 1,4)])) stop 2
  if (any (f (3.0, 1, 4.0) .ne. [(real(i), i = 1,2)])) stop 3

  r = [(real (i), i = 10,20)]
  call g (5, r)
  if (int (sum (r)) /= (sum ([(i, i = 15,20)]) + 5)) stop 4
end
