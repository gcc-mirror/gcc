! { dg-do run }
!
! Test the fix for PR80120
!
! Contributed by Marco Restelli  <mrestelli@gmail.com>
!
program p
 implicit none

 type :: t
  character(len=25) :: text(2)
 end type t
 type(t) :: x

 x%text(1) = "ABC"
 x%text(2) = "defgh"

 associate( c => x%text )
   if (c(1)(:maxval(len_trim(c))) .ne. trim (x%text(1))) call abort
   if (c(2)(:maxval(len_trim(c))) .ne. trim (x%text(2))) call abort
 end associate

end program p
