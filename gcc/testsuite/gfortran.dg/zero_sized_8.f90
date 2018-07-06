! { dg-do run }
program p
   complex, parameter :: a(0) = 0
   real, parameter :: x(0) = 0
   integer, parameter :: z(0) = 0
   if (any(z > 0) .neqv. .false.)         stop 1
   if (all(z > 0) .neqv. .true.)          stop 2
   if (count(z > 0) /= 0)                 stop 3
   if (kind(count(z > 0, kind=1)) /= 1)   stop 4
   if (iall(z) /= not(int(0, kind(z))))   stop 5
   if (iany(z) /= 0)                      stop 6
   if (iparity(z) /= 0)                   stop 7
   if (maxval(z) /= -huge(0) - 1)         stop 8
   if (maxval(x) /= -huge(x))             stop 9
   if (minval(z) /= huge(0))              stop 10
   if (minval(x) /= huge(x))              stop 11
   if (norm2(x) /= 0)                     stop 12
   if (real(product(a)) /= 1 .and. aimag(product(a)) /= 0) stop 13
   if (product(x) /= 1)                   stop 14
   if (product(z) /= 1)                   stop 15
   if (real(sum(a)) /= 0 .and. aimag(sum(a)) /= 0) stop 13
   if (sum(x) /= 0)                       stop 14
   if (sum(z) /= 0)                       stop 15
   call q
end

subroutine q
   complex, parameter :: a(0) = 0
   real, parameter :: x(3,4,0) = 0
   integer, parameter :: z(3,4,0) = 0
   if (any(z > 0) .neqv. .false.)         stop 101
   if (all(z > 0) .neqv. .true.)          stop 102
   if (count(z > 0) /= 0)                 stop 103
   if (kind(count(z > 0, kind=1)) /= 1)   stop 104
   if (iall(z) /= not(int(0, kind(z))))   stop 105
   if (iany(z) /= 0)                      stop 106
   if (iparity(z) /= 0)                   stop 107
   if (maxval(z) /= -huge(0) - 1)         stop 108
   if (maxval(x) /= -huge(x))             stop 109
   if (minval(z) /= huge(0))              stop 110
   if (minval(x) /= huge(x))              stop 111
   if (norm2(x) /= 0)                     stop 112
   if (real(product(a)) /= 1 .and. aimag(product(a)) /= 0) stop 113
   if (product(x) /= 1)                   stop 114
   if (product(z) /= 1)                   stop 115
   if (real(sum(a)) /= 0 .and. aimag(sum(a)) /= 0) stop 13
   if (sum(x) /= 0)                   stop 14
   if (sum(z) /= 0)                   stop 15
end
! { dg-prune-output "symmetric range implied by Standard" }
