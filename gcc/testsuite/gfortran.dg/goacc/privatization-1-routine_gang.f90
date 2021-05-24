! OpenACC privatization: 'routine'

! { dg-additional-options "-fopt-info-omp-note" }
! { dg-additional-options "--param=openacc-privatization=noisy" } for
! testing/documenting aspects of that functionality.

! See also '../../c-c++-common/goacc/privatization-1-routine_gang.c'.
!TODO More cases should be added here.

! It's only with Tcl 8.5 (released in 2007) that "the variable 'varName'
! passed to 'incr' may be unset, and in that case, it will be set to [...]",
! so to maintain compatibility with earlier Tcl releases, we manually
! initialize counter variables:
! { dg-line l_dummy[variable c_routine 0] }
! { dg-message "dummy" "" { target iN-VAl-Id } l_dummy } to avoid
! "WARNING: dg-line var l_dummy defined, but not used".

module m
contains
  subroutine f (i, j, a)
    implicit none
    integer :: i, j, a
    !$acc routine (f) gang ! { dg-line l_routine[incr c_routine] }
    integer :: x, y
    integer, parameter :: c = 3
    integer, external :: g

          ! Can't have nested scopes fun.  (Fortran 'block' construct supported only starting with OpenACC 3.1.)

          ! Don't know how to effect a 'LABEL_DECL' here.
          ! Don't know how to effect a 'TYPE_DECL' here.
          ! Don't know how to effect a 'FUNCTION_DECL' here.
          ! Don't know how to effect a 'RESULT_DECL' here.
          ! Don't know how to effect a 'VAR_DECL' here.
          ! (See C/C++ example.)

          a = g (i, j, a, c)
          x = a
          !$acc atomic write ! ... to force 'TREE_ADDRESSABLE'.
          y = a
  end subroutine f
    ! { dg-note {variable 'i' in 'private' clause potentially has improper OpenACC privatization level: 'parm_decl'} "TODO" { xfail *-*-* } l_routine$c_routine }
    ! { dg-note {variable 'j' in 'private' clause potentially has improper OpenACC privatization level: 'parm_decl'} "TODO" { xfail *-*-* } l_routine$c_routine }
    ! { dg-note {variable 'a' in 'private' clause potentially has improper OpenACC privatization level: 'parm_decl'} "TODO" { xfail *-*-* } l_routine$c_routine }
    ! { dg-note {variable 'C\.[0-9]+' declared in block potentially has improper OpenACC privatization level: 'const_decl'} "TODO" { xfail *-*-* } l_routine$c_routine }
    ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "TODO" { xfail *-*-* } l_routine$c_routine }
end module m
