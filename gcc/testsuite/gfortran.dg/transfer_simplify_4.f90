! { dg-do run }
! { dg-options "-O0" }
! Tests that the in-memory representation of a transferred variable
! propagates properly.
!
  implicit none

  integer, parameter :: ip1 = 42
  logical, parameter :: ap1 = transfer(ip1, .true.)
  integer, parameter :: ip2 = transfer(ap1, 0)

  logical :: a
  integer :: i
  
  i = transfer(transfer(ip1, .true.), 0)
  if (i .ne. ip1) call abort ()

  i = transfer(ap1, 0)
  if (i .ne. ip1) call abort ()
  
  a = transfer(ip1, .true.)
  i = transfer(a, 0)
  if (i .ne. ip1) call abort ()

  i = ip1
  a = transfer(i, .true.)
  i = transfer(a, 0)
  if (i .ne. ip1) call abort ()

end
