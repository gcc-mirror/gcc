! { dg-do compile }
! Tests the fix for 26074, in which the array reference below would
! be determined not to be constant within modules.
!
! Contributed by Jonathan Dursi  <ljdursi@cita.utoronto.ca>
!
module foo

   integer, parameter :: len = 5
   integer :: arr(max(len,1))

end
