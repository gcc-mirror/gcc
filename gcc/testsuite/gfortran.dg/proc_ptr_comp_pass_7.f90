! { dg-do compile }
!
! PR 46662: [OOP] gfortran accepts "CALL polymorphic%abstract_type%ppc()"
!
! Contributed by Wolfgang Kilian <kilian@hep.physik.uni-siegen.de>
! cf. http://groups.google.com/group/comp.lang.fortran/browse_thread/thread/a0857fa4a692d518

module types
  implicit none

  type, abstract :: base_t
     integer :: i = 0
     procedure(base_write_i), pointer :: write_procptr
   contains
     procedure :: write_i => base_write_i
  end type base_t

  type, extends (base_t) :: t
  end type t

contains

  subroutine base_write_i (obj)
    class (base_t), intent(in) :: obj
    print *, obj%i
  end subroutine base_write_i

end module types


program main
  use types
  implicit none

  type(t) :: obj

  print *, "Direct printing"
  obj%i = 1
  print *, obj%i

  print *, "Direct printing via parent"
  obj%base_t%i = 2
  print *, obj%base_t%i

  print *, "Printing via TBP"
  obj%i = 3
  call obj%write_i

  print *, "Printing via parent TBP"
  obj%base_t%i = 4
  call obj%base_t%write_i      ! { dg-error "is of ABSTRACT type" }

  print *, "Printing via OBP"
  obj%i = 5
  obj%write_procptr => base_write_i
  call obj%write_procptr

  print *, "Printing via parent OBP"
  obj%base_t%i = 6
  obj%base_t%write_procptr => base_write_i
  call obj%base_t%write_procptr               ! { dg-error "is of ABSTRACT type" }

end program main
