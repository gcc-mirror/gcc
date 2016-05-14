! { dg-do compile }
! { dg-options "-fdec-structure" }
!
! Basic compile tests for what CAN be done with dot ('.') as a member accessor.
!

logical :: l, l2 = .true., l3 = .false., and
integer i
character(5) s
real r

structure /s1/
  integer i
  character(5) s
  real r
end structure

record /s1/ r1

! Basic 
l = l .and. l2 .or. l3
l = and .and. and .and. and
l =  l2 .eqv. l3
l = (l2) .eqv. l3

! Integers
l = .not. (i .eq. 0)
l = .not. (0 .eq. i)
l = .not. (r1.i .eq. 0)
l = .not. (0 .eq. r1.i)
! Characters
l = .not. (s .eq. "hello")
l = .not. ("hello" .eq. s)
l = .not. (r1.s .eq. "hello")
l = .not. ("hello" .eq. r1.s)
! Reals
l = .not. (r .eq. 3.14)
l = .not. (3.14 .eq. r)
l = .not. (r1.r .eq. 3.14)
l = .not. (3.14 .eq. r1.r)

end
