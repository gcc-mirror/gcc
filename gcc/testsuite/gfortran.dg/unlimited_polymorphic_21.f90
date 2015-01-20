! { dg-do run }
! Tests the fix for PR64578.
!
! Contributed by Damian Rouson  <damian@sourceryinstitute.org>
!
  type foo
     real, allocatable :: component(:)
  end type
  type (foo), target :: f
  class(*), pointer :: ptr(:)
  allocate(f%component(1),source=[0.99])
  call associate_pointer(f,ptr)
  select type (ptr)
    type is (real)
      if (abs (ptr(1) - 0.99) > 1e-5) call abort
  end select
  ptr => return_pointer(f)  ! runtime segmentation fault
  if (associated(return_pointer(f)) .neqv. .true.) call abort
  select type (ptr)
    type is (real)
      if (abs (ptr(1) - 0.99) > 1e-5) call abort
  end select
contains
  subroutine associate_pointer(this, item)
    class(foo), target :: this
    class(*), pointer :: item(:)
    item => this%component
  end subroutine
  function return_pointer(this)
    class(foo), target :: this
    class(*), pointer :: return_pointer(:)
    return_pointer => this%component
  end function
end

