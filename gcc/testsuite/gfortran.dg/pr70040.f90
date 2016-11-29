! { dg-do compile }
! PR 70040 - used to cause an ICE.
! Test case by Martin Reinecke
program bugrep
  implicit none
  type :: string
    character (len=:), allocatable :: s
  end type

  integer l
  type(string), allocatable, dimension(:) :: foo
  character(len=:),allocatable ::tmp
  allocate(foo(20))
  do l= 1, 20
    tmp = foo(5)%s
    foo(5)%s = foo(l)%s
    foo(l)%s = tmp
  enddo
end program
