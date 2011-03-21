! { dg-do compile }
!
! PR fortran/47042
!
! Contribued by Jerry DeLisle
!

program bug

contains
function get_cstring ()
  character              :: get_cstring
  character, pointer     :: ptmp
  character, allocatable :: atmp

  get_cstring = ptmp(i) ! { dg-error "must have an explicit function interface" }
  get_cstring = atmp(i) ! { dg-error "must have an explicit function interface" }
end function

function get_cstring2 ()
  EXTERNAL :: ptmp, atmp
  character              :: get_cstring2
  character, pointer     :: ptmp
  character, allocatable :: atmp

  get_cstring2 = atmp(i) ! { dg-error "must have an explicit function interface" }

  ! The following is regarded as call to a procedure pointer,
  ! which is in principle valid:
  get_cstring2 = ptmp(i)
end function

end program
