! { dg-do run }
! Tests the fix for PR33370, in which array copying, with subreferences
! was broken due to a regression.
!
! Reported by Thomas Koenig <tkoenig@gcc.gnu.org>
!
program main
  type foo
    integer :: i
     character(len=3) :: c
  end type foo
  type(foo), dimension(2) :: a = (/foo (1, "uvw"), foo (2, "xyz")/)
  type(foo), dimension(2) :: b = (/foo (101, "abc"), foo (102, "def")/)
  a%i = 0
  print *, a
  a%i = (/ 12, 2/)
  if (any (a%c .ne. (/"uvw", "xyz"/))) call abort ()
  if (any (a%i .ne. (/12, 2/))) call abort ()
  a%i = b%i
  if (any (a%c .ne. (/"uvw", "xyz"/))) call abort ()
  if (any (a%i .ne. (/101, 102/))) call abort ()
end program main
