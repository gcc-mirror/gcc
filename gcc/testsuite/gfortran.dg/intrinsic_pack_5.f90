! { dg-do run }
!
! PR 41478: Corrupted memory using PACK for derived-types with allocated components
! PR 42268: [4.4/4.5 Regression] derived type segfault with pack
!
! Original test case by Juergen Reuter <reuter@physik.uni-freiburg.de>
! Modified by Janus Weil <janus@gcc.gnu.org>

type :: container_t
  integer:: entry = -1
end type container_t
type(container_t), dimension(1) :: a1, a2
a2(1)%entry = 1
a1 = pack (a2, mask = [.true.])
if (a1(1)%entry/=1) STOP 1
end
