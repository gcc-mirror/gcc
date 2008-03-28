! { dg-do compile }
!
! PR fortran/34714 - ICE on invalid
! Testcase contributed by Martin Reinecke <martin AT mpa-garching DOT mpg DOT de>
!

module foo
  type bar
    logical, pointer, dimension(:) :: baz
  end type
contains

function func1()
  type(bar) func1
  allocate(func1%baz(1))
end function

function func2()
  type(bar) func2
  allocate(func1%baz(1))      ! { dg-error "is not a variable" }
end function

end module foo

! { dg-final { cleanup-modules "foo" } }
