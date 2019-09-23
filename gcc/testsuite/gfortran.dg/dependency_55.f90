! { dg-do run }
!
! Test the fix for PR91717 in which the concatenation operation ICEd.
!
! Contributed by Damian Rouson  <damian@sourceryinstitute.org>
!
  type core
    character (len=:), allocatable :: msg
  end type

  type(core) :: my_core

  my_core%msg = ""
  my_core%msg = my_core%msg//"my message is: "
  my_core%msg = my_core%msg//"Hello!"

  if (my_core%msg .ne. "my message is: Hello!") stop 1
end
