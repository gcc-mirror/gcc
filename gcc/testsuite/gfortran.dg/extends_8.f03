! { dg-do compile }
!
! PR 41784: [OOP] ICE in load_derived_extensions
!
! Contributed by Salvatore Filippone <sfilippone@uniroma2.it>

module m
  type  :: A
  end type
  type, extends(A) :: B
  end type
end module

use m, only: A
end
 
