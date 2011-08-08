! { dg-do run }
! Test the fix for a regression caused by the fix for PR33541,
! in which the second local version of a would not be associated.
!
! Contributed by Dominique d'Humieres <dominiq@lps.ens.fr>
!            and Tobias Burnus <burnus@gcc.gnu.org>
!
module m
  integer :: a
end module m

use m, local1 => a
use m, local2 => a
local1 = 5
local2 = 3
if (local1 .ne. local2) call abort ()
end
! { dg-final { cleanup-modules "m" } }
