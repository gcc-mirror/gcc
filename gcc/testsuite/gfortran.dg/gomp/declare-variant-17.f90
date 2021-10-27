! { dg-do compile }

! Declare variant directives should only appear in the specification parts.

program main
  implicit none

  continue

  !$omp declare variant (base: variant) match (construct={parallel})  ! { dg-error "Unexpected \\\!\\\$OMP DECLARE VARIANT statement at .1." }
contains
  subroutine base ()
    continue

    !$omp declare variant (variant) match (construct={parallel})  ! { dg-error "Unexpected \\\!\\\$OMP DECLARE VARIANT statement at .1." }
  end subroutine
end program
