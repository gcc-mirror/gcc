! { dg-do compile }
!
! PR fortran/46205
!
! Contributed by Jonathan Stott
!

program forallBug
  logical :: valid(4) = (/ .true., .true., .false., .true. /)
  real    :: vec(4)
  integer :: j

  ! This is an illegal statement.  It should read valid(j), not valid.
  forall (j = 1:4, valid) ! { dg-error "requires a scalar LOGICAL expression" }
     vec(j) = sin(2*3.14159/j)
  end forall
end program forallBug
