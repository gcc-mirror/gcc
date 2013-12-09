! { dg-do compile }
!
! PR 41733: Proc-pointer conformance checks: Elemental-proc-ptr => non-elemental-procedure
!
! Contributed by James Van Buskirk

  implicit none
  procedure(my_dcos), pointer :: f ! { dg-error "Procedure pointer 'f' at .1. shall not be elemental" }
  f => my_dcos           ! { dg-error "Nonintrinsic elemental procedure 'my_dcos' is invalid in procedure pointer assignment" }
contains
  real elemental function my_dcos(x)
    real, intent(in) :: x
    my_dcos = cos(x)
  end function
end
