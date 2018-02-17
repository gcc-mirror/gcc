! { dg-do run }
! This test the fix of PR18283, where assignments of scalar,
! character pointer components of derived types caused an ICE.
! It also checks that the array counterparts remain operational.
! Contributed by Paul Thomas  pault@gcc.gnu.org
!
program char_pointer_comp_assign
  implicit none
  type :: dt
     character (len=4), pointer :: scalar
     character (len=4), pointer :: array(:)
  end type dt
  type (dt) :: a 
  character (len=4), target :: scalar_t ="abcd"
  character (len=4), target :: array_t(2) = (/"abcd","efgh"/)

! Do assignments first
  allocate (a%scalar, a%array(2))
  a%scalar = scalar_t
  if (a%scalar /= "abcd") STOP 1
  a%array = array_t
  if (any(a%array /= (/"abcd","efgh"/))) STOP 2
  deallocate (a%scalar, a%array)

! Now do pointer assignments.
  a%scalar => scalar_t
  if (a%scalar /= "abcd") STOP 3
  a%array => array_t
  if (any(a%array /= (/"abcd","efgh"/))) STOP 4

end program char_pointer_comp_assign
