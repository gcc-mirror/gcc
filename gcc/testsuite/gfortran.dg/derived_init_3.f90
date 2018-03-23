! { dg-do run }
!
! PR fortran/40851
!
! Make sure the an INTENT(OUT) dummy is not initialized
! when it is a pointer.
!
! Contributed by Juergen Reuter <juergen.reuter@desy.de>.
!
program main

  type :: string
     character,dimension(:),allocatable :: chars
  end type string

  type :: string_container
     type(string) :: string
  end type string_container

  type(string_container), target :: tgt
  type(string_container), pointer :: ptr

  ptr => tgt
  call set_ptr (ptr)
  if (associated(ptr)) STOP 1

contains

  subroutine set_ptr (ptr)
    type(string_container), pointer, intent(out) :: ptr
    ptr => null ()
  end subroutine set_ptr

end program main
