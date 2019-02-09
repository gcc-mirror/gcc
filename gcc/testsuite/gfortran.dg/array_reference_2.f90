! { dg-do run }
!
! Test the fix for PR89200, in which the indexing did not work in
! the write statement below.
!
! Contributed by Damian Rouson  <damian@sourceryinstitute.org>
!
  type foo
    character(len=:), allocatable :: string
  end type
  type foo_list
    type(foo), allocatable :: entry(:)
  end type
  type(foo_list) list
  character(4) :: buffer
  list = foo_list([foo('12'), foo('34')])
  write(buffer, '(2a2)') list%entry(1)%string, list%entry(2)%string
  if (buffer .ne. '1234') stop 1
  deallocate (list%entry)
end
