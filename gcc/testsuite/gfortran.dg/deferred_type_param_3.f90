! { dg-do compile }
!
! PR fortran/45170
! PR fortran/52158
!
! Contributed by Damian Rouson

module speaker_class
  type speaker
  contains
    procedure :: speak
  end type
contains
  function speak(this)
    class(speaker) ,intent(in) :: this
    character(:) ,allocatable :: speak
  end function
  subroutine say_something(somebody)
    class(speaker) :: somebody
    print *,somebody%speak()
  end subroutine
end module

