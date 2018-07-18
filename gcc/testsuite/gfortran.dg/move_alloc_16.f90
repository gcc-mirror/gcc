! { dg-do run }
!
! Tests the fix for PR67177 in which MOVE_ALLOC was not assigning the string
! length for deferred length characters.
!
! Contributed by <templed@tcd.ie>
!
program str
  implicit none

  type string
    character(:), Allocatable :: text
  end type string

  type strings
    type(string), allocatable, dimension(:) :: strlist
  end type strings

  type(strings) :: teststrs
  type(string) :: tmpstr
  integer :: strlen = 20

  allocate (teststrs%strlist(1))
  allocate (character(len=strlen) :: tmpstr%text)

  allocate (character(len=strlen) :: teststrs%strlist(1)%text)

! Full string reference was required because reallocation on assignment is
! functioning when it should not if the lhs is a substring - PR67977
  tmpstr%text(1:3) = 'foo'

  if (.not.allocated (teststrs%strlist(1)%text)) STOP 1
  if (len (tmpstr%text) .ne. strlen) STOP 2

  call move_alloc(tmpstr%text,teststrs%strlist(1)%text)

  if (.not.allocated (teststrs%strlist(1)%text)) STOP 3
  if (len (teststrs%strlist(1)%text) .ne. strlen) STOP 4
  if (trim (teststrs%strlist(1)%text(1:3)) .ne. 'foo') STOP 5

! Clean up so that valgrind reports all allocated memory freed.
  if (allocated (teststrs%strlist(1)%text)) deallocate (teststrs%strlist(1)%text)
  if (allocated (teststrs%strlist)) deallocate (teststrs%strlist)
end program str
