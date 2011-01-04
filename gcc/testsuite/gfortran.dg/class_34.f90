! { dg-do compile }
!
! PR 46448: [4.6 Regression] [OOP] symbol `__copy_...' is already defined
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

module m0
  type :: t
  end type
end module 

module m1
  use m0
  class(t), pointer :: c1
end module

module m2
  use m0
  class(t), pointer :: c2
end module

end

! { dg-final { cleanup-modules "m0 m1 m2" } }
