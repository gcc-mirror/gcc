! { dg-do run }
!
! Test fix for PR80945, in which the character length was fixed at zero.
!
! Contributed by Nicolas Koenig  <koenigni@gcc.gnu.org>
!
program main
    implicit none
    integer:: i
    integer, parameter:: N = 10
    character(20) :: buffer
    character(len=:), dimension(:),allocatable:: ca
    character(len=:), dimension(:,:),allocatable:: cb
    allocate(character(len=N) :: ca(3))
    ca(1) = "foo"
    ca(2) = "bar"
    ca(3) = "xyzzy"
    write (buffer, '(3A5)') ca(1:3)
    if (trim (buffer) .ne. "foo  bar  xyzzy") stop 1
end program
