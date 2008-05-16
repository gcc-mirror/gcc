! { dg-do compile }
subroutine bug1
  integer, parameter :: ik1 = 1, ik2 = 2
  integer, parameter :: i = kind(int((0.,0.), kind=ik1))
  integer, parameter :: j = kind(int((0.,0.), kind=ik2))
  integer, parameter :: k = kind(int(0.,      kind=ik1))
  integer, parameter :: l = kind(int(0.,      kind=ik2))
  integer, parameter :: m = kind(int(0,       kind=ik1))
  integer, parameter :: n = kind(int(0,       kind=ik2))
end subroutine bug1
