module np
  implicit none
  integer, parameter :: za = selected_real_kind(15, 307)
end module np

module gg
  use np

  type et(real_kind)
    integer, kind :: real_kind
  end type et

contains

  function hv (tm) result(ce)
    type (et(real_kind=za)), allocatable, target :: tm
    type (et(real_kind=za)), pointer :: ce

    allocate (tm) ! { dg-bogus "dereference of possibly-NULL" }
    ce => tm
  end function hv

end module gg

program a5
  use np
  use gg
  implicit none
  type (et(real_kind=za)), allocatable :: qb
  type (et(real_kind=za)), pointer :: vt

  vt => hv (qb)
end program a5 ! { dg-warning "leak of '.*qb'" }
