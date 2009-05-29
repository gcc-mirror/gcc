! { dg-do compile }
! { dg-options "-O -Wuninitialized" }

module foo
implicit none

  type bar
    integer :: yr
  end type

contains

  function baz(arg) result(res) ! { dg-warning "res.yr' may be" "" { xfail *-*-* } }
    type(bar), intent(in) :: arg
    type(bar) :: res
    logical, external:: some_func
    if (.not. some_func(arg)) then
      call fatal('arg not valid')
    else
      res = arg
    end if
  end function baz

end module foo

! { dg-final { cleanup-modules "foo" } }
