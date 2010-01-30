! { dg-do compile }
! Test the fix for pr41167, in which the first argument of 'pack', below,
! was simplified incorrectly, with the results indicated.
!
! Contributed by Harald Anlauf <anlauf@gmx.de>
!
program gfcbug88
  implicit none
  type t
     character(len=8) :: name
  end type t
  type(t) ,parameter :: obstyp(2)= (/ t ('A'), t ('B') /)
  character(9) :: chr(1)

  print *, pack (" "//obstyp(:)% name, (/ .true., .false. /))  ! Used to ICE on compilation
  chr = pack (" "//obstyp(:)% name, (/ .true., .false. /))  ! Used to give conversion error
end program gfcbug88
