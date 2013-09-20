! { dg-do compile }
!
! PR 47745: [OOP] Segfault with CLASS(*) and derived type dummy arguments
!
! Contributed by Rodney Polkinghorne <thisrod@gmail.com>

  type, abstract :: T 
  end type T
contains
  class(T) function add()  ! { dg-error "must be dummy, allocatable or pointer" }
    add = 1  ! { dg-error "Nonallocatable variable must not be polymorphic in intrinsic assignment" }
  end function
end
