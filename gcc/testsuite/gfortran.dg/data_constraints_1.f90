! { dg-do compile }
! { dg-options "" }
! Tests standard indepedendent constraints for variables in a data statement
!
! Contributed by Paul Thomas <pault@gcc.gnu.org>
!
  module global
   integer n
  end module global

  use global
  integer q
  data n /0/            ! { dg-error "Cannot change attributes" }
  n = 1
  n = foo (n)
contains
  function foo (m) result (bar)
  integer p (m), bar
  integer, allocatable :: l(:)
  allocate (l(1))
  data l /42/           ! { dg-error "conflicts with ALLOCATABLE" }
  data p(1) /1/         ! { dg-error "non-constant array in DATA" }
  data q /1/            ! { dg-error "Host associated variable" }
  data m /1/            ! { dg-error "conflicts with DUMMY attribute" }
  data bar /99/         ! { dg-error "conflicts with RESULT" }
  end function foo
  function foobar ()
  integer foobar
  data foobar /0/       ! { dg-error "conflicts with FUNCTION" }
  end function foobar
end
