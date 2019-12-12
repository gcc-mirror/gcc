! { dg-do compile }
!
! Test the fix for PR86906, in which a spurious error was generated
! by 'config' in the subroutine having the same symbol name as the
! renamed 'foo_config'.
!
! Contributed by Damian Rouson  <damian@sourceryinstitute.org>
!
module foo
  type config
  end type
end module
  use foo, only: foo_config => config
contains
  subroutine cap
    integer config
    type(foo_config) extra
  end subroutine
end
