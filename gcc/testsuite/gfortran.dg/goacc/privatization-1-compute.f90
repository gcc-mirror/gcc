! OpenACC privatization: compute construct

! { dg-additional-options "-fopt-info-omp-note" }
! { dg-additional-options "--param=openacc-privatization=noisy" } for
! testing/documenting aspects of that functionality.

! { dg-additional-options -Wuninitialized }

! See also '../../c-c++-common/goacc/privatization-1-compute.c'.
!TODO More cases should be added here.

! It's only with Tcl 8.5 (released in 2007) that "the variable 'varName'
! passed to 'incr' may be unset, and in that case, it will be set to [...]",
! so to maintain compatibility with earlier Tcl releases, we manually
! initialize counter variables:
! { dg-line l_dummy[variable c_compute 0 c_function 0] }
! { dg-message "dummy" "" { target iN-VAl-Id } l_dummy } to avoid
! "WARNING: dg-line var l_dummy defined, but not used".

module m
contains
  subroutine f (i, j, a) ! { dg-line l_function[incr c_function] }
    implicit none
    integer :: i, j, a
    integer :: x, y
    integer, parameter :: c = 3
    integer, external :: g

    !$acc parallel private(i, j, a) private(x, y) ! { dg-line l_compute[incr c_compute] }
          ! Can't have nested scopes fun.  (Fortran 'block' construct supported only starting with OpenACC 3.1.)

          ! Don't know how to effect a 'LABEL_DECL' here.
          ! Don't know how to effect a 'TYPE_DECL' here.
          ! Don't know how to effect a 'FUNCTION_DECL' here.
          ! Don't know how to effect a 'RESULT_DECL' here.
          ! Don't know how to effect a 'VAR_DECL' here.
          ! (See C/C++ example.)

          a = g (i, j, a, c)
          ! { dg-warning {'i' is used uninitialized} {} { target *-*-* } .-1 }
          !   { dg-note {'i' was declared here} {} { target *-*-* } l_function$c_function }
          ! { dg-warning {'j' is used uninitialized} {} { target *-*-* } .-3 }
          !   { dg-note {'j' was declared here} {} { target *-*-* } l_function$c_function }
          ! { dg-warning {'a' is used uninitialized} {} { target *-*-* } .-5 }
          !   { dg-note {'a' was declared here} {} { target *-*-* } l_function$c_function }
          x = a
          !$acc atomic write ! ... to force 'TREE_ADDRESSABLE'.
          y = a
    !$acc end parallel
    ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "TODO" { xfail *-*-* } l_compute$c_compute }
    ! { dg-note {variable 'j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "TODO" { xfail *-*-* } l_compute$c_compute }
    ! { dg-note {variable 'a' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "TODO" { xfail *-*-* } l_compute$c_compute }
    ! { dg-note {variable 'C\.[0-9]+' declared in block potentially has improper OpenACC privatization level: 'const_decl'} "TODO" { target *-*-* } l_compute$c_compute }
  end subroutine f
end module m
