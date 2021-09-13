! OpenACC privatization: 'loop' construct

! { dg-additional-options "-fopt-info-omp-note" }
! { dg-additional-options "--param=openacc-privatization=noisy" } for
! testing/documenting aspects of that functionality.

! See also '../../c-c++-common/goacc/privatization-1-compute-loop.c'.
!TODO More cases should be added here.

! It's only with Tcl 8.5 (released in 2007) that "the variable 'varName'
! passed to 'incr' may be unset, and in that case, it will be set to [...]",
! so to maintain compatibility with earlier Tcl releases, we manually
! initialize counter variables:
! { dg-line l_dummy[variable c_loop 0] }
! { dg-message "dummy" "" { target iN-VAl-Id } l_dummy } to avoid
! "WARNING: dg-line var l_dummy defined, but not used".

module m
contains
  subroutine f (i, j, a)
    implicit none
    integer :: i, j, a
    integer :: x, y
    integer, parameter :: c = 3
    integer, external :: g

    !$acc parallel
    !$acc loop collapse(2) private(a) private(x, y) ! { dg-line l_loop[incr c_loop] }
    do i = 1, 20
       do j = 1, 25
          ! Can't have nested scopes fun.  (Fortran 'block' construct supported only starting with OpenACC 3.1.)

          ! Don't know how to effect a 'LABEL_DECL' here.
          ! Don't know how to effect a 'TYPE_DECL' here.
          ! Don't know how to effect a 'FUNCTION_DECL' here.
          ! Don't know how to effect a 'RESULT_DECL' here.
          ! Don't know how to effect a 'VAR_DECL' here.
          ! (See C/C++ example.)

          a = g (i, j, a, c)
          x = a
          !$acc atomic write
          y = a
       end do
    end do
    ! { dg-note {variable 'count\.[0-9]+' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop$c_loop }
    ! { dg-note {variable 'i' in 'private' clause potentially has improper OpenACC privatization level: 'parm_decl'} "TODO" { target *-*-* } l_loop$c_loop }
    ! { dg-note {variable 'j' in 'private' clause potentially has improper OpenACC privatization level: 'parm_decl'} "TODO" { target *-*-* } l_loop$c_loop }
    ! { dg-note {variable 'a' in 'private' clause potentially has improper OpenACC privatization level: 'parm_decl'} "TODO" { target *-*-* } l_loop$c_loop }
    ! { dg-note {variable 'x' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop$c_loop }
    ! { dg-note {variable 'y' in 'private' clause is candidate for adjusting OpenACC privatization level} "" { target *-*-* } l_loop$c_loop }
    ! { dg-note {variable 'C\.[0-9]+' declared in block potentially has improper OpenACC privatization level: 'const_decl'} "TODO" { target *-*-* } l_loop$c_loop }
    ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop$c_loop }
    ! { dg-note {variable 'y' ought to be adjusted for OpenACC privatization level: 'vector'} "" { target *-*-* } l_loop$c_loop }
    !$acc end parallel
  end subroutine f
end module m
