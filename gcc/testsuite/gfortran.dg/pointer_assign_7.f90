! { dg-do compile }
!
! PR 39931: ICE on invalid Fortran 95 code (bad pointer assignment)
!
! Contributed by Thomas Orgis <thomas.orgis@awi.de>

program point_of_no_return

implicit none

type face_t
  integer :: bla
end type

integer, pointer :: blu
type(face_t), pointer :: face

allocate(face)
allocate(blu)

face%bla => blu  ! { dg-error "Non-POINTER in pointer association context" }

end program

