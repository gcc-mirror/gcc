! { dg-do compile }
! Tests the fix for PR29060 in which the shape of the result
! of SPREAD was not available to the scalarizer.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
  real,dimension(:, :),pointer :: ptr
  real,dimension(2, 2) :: u

  u = reshape((/0.25, 0.5, 0.75, 1.00/),(/2,2/))
  
  allocate (ptr(2,2))

! Original PR
  ptr(:, :) = u + spread ((/1.0, 2.0/), 2, size(u, 2))
  if (any (ptr .ne. &
        reshape ((/1.25, 2.50, 1.75, 3.00/), (/2, 2/)))) call abort ()

! Check that the fix works correctly with the source shape after ncopies
  ptr(:, :) = u + spread ((/2.0, 3.0/), 1, size (u, 1))
  if (any (ptr .ne. &
        reshape ((/2.25, 2.50, 3.75, 4.00/), (/2,2/)))) call abort ()
end
