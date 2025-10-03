! { dg-do run }
! { dg-additional-options "-O2" }
!
! PR fortran/107968
!
! Verify that array I/O optimization is not used for a section
! of an array pointer as the pointee can be non-contiguous
!
! Contributed by Nils Dreier

PROGRAM foo
  implicit none

  TYPE t_geographical_coordinates
     REAL :: lon
     REAL :: lat
  END TYPE t_geographical_coordinates

  TYPE t_vertices
     REAL, POINTER          :: vlon(:) => null()
     REAL, POINTER          :: vlat(:) => null()
  END TYPE t_vertices

  TYPE(t_geographical_coordinates), TARGET :: vertex(2)
  TYPE(t_vertices), POINTER :: vertices_pointer
  TYPE(t_vertices), TARGET  :: vertices_target

  character(24)           :: s0, s1, s2
  character(*), parameter :: fmt = '(2f8.3)'

  ! initialization
  vertex%lon = [1,3]
  vertex%lat = [2,4]

  ! obtain pointer to (non-contiguous) field
  vertices_target%vlon => vertex%lon

  ! reference output of write
  write (s0,fmt) vertex%lon

  ! set pointer vertices_pointer in a subroutine
  CALL set_vertices_pointer(vertices_target)

  write (s1,fmt) vertices_pointer%vlon
  write (s2,fmt) vertices_pointer%vlon(1:)
  if (s1 /= s0 .or. s2 /= s0) then
     print *, s0, s1, s2
     stop 3
  end if

CONTAINS

  SUBROUTINE set_vertices_pointer(vertices)
    TYPE(t_vertices), POINTER, INTENT(IN) :: vertices

    vertices_pointer => vertices

    write (s1,fmt) vertices        %vlon
    write (s2,fmt) vertices        %vlon(1:)
    if (s1 /= s0 .or. s2 /= s0) then
       print *, s0, s1, s2
       stop 1
    end if

    write (s1,fmt) vertices_pointer%vlon
    write (s2,fmt) vertices_pointer%vlon(1:)
    if (s1 /= s0 .or. s2 /= s0) then
       print *, s0, s1, s2
       stop 2
    end if
  END SUBROUTINE set_vertices_pointer
END PROGRAM foo
