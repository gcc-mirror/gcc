! { dg-do run }

program p
   logical, parameter :: a(0) = .true.
   real, parameter :: b(0) = 0
   complex, parameter :: c(0) = 0
   integer :: d
   data d /a%kind/
   data e /b%kind/
   data f /c%kind/
   if (d .ne. kind(a)) stop 1
   if (e .ne. kind(b)) stop 2
   if (f .ne. kind(c)) stop 3
end

