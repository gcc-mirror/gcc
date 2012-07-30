! { dg-do compile }
!
! PR 41733: Proc-pointer conformance checks: Elemental-proc-ptr => non-elemental-procedure
!
! Contributed by James Van Buskirk

  implicit none
  procedure(my_dcos), pointer :: f
  f => my_dcos           ! { dg-error "invalid in procedure pointer assignment" }
contains
  real elemental function my_dcos(x)
    real, intent(in) :: x
    my_dcos = cos(x)
  end function
end
