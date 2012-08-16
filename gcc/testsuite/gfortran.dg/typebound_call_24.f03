! { dg-do compile }
!
! PR 54243: [OOP] ICE (segfault) in gfc_type_compatible for invalid BT_CLASS
!
! Contributed by Sylwester Arabas <slayoo@staszic.waw.pl>

module aqq_m
  type :: aqq_t
  contains
    procedure :: aqq_init
  end type 
 contains
  subroutine aqq_init(this)
    class(aqq_t) :: this
  end subroutine
end module

program bug2
  use aqq_m
  class(aqq_t) :: aqq  ! { dg-error "must be dummy, allocatable or pointer" }
  call aqq%aqq_init
end program

! { dg-final { cleanup-modules "aqq_m" } }
