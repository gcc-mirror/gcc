! { dg-do compile }
! Tests the fix for PR29464, in which the second USE of the generic
! interface caused an error.
!
! Contributed by Vivek Rao <vivekrao4@yahoo.com>
!
module foo_mod
  implicit none
  interface twice
     module procedure twice_real
  end interface twice
contains
  real function twice_real(x)
    real :: x
    twice_real = 2*x
  end function twice_real
end module foo_mod

  subroutine foobar ()
    use foo_mod, only: twice, twice
    print *, twice (99.0)
  end subroutine foobar

  program xfoo
  use foo_mod, only: two => twice, dbl => twice
  implicit none
  call foobar ()
  print *, two (2.3)
  print *, dbl (2.3)
end program xfoo
! { dg-final { cleanup-modules "foo_mod" } }
