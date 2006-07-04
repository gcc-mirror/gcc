! { dg-do run }
! Tests the fix for pr28174, in which the fix for pr28118 was
! corrupting the character lengths of arrays that shared a
! character length structure.  In addition, in developing the
! fix, it was noted that intent(out/inout) arguments were not
! getting written back to the calling scope.
!
! Based on the testscase by Harald Anlauf  <anlauf@gmx.de>
!
program pr28174
  implicit none
  character(len=12) :: teststring(2) = (/ "abc def ghij", &
                                          "klm nop qrst" /)
  character(len=12) :: a(2), b(2), c(2), d(2)
  integer :: m = 7, n
  a = teststring
  b = a
  c = a
  d = a
  n = m - 4

! Make sure that variable substring references work.
  call foo (a(:)(m:m+5), c(:)(n:m+2), d(:)(5:9))
  if (any (a .ne. teststring)) call abort ()
  if (any (b .ne. teststring)) call abort ()
  if (any (c .ne. (/"ab456789#hij", &
                    "kl7654321rst"/))) call abort ()
  if (any (d .ne. (/"abc 23456hij", &
                    "klm 98765rst"/))) call abort ()
contains
  subroutine foo (w, x, y)
    character(len=*), intent(in) :: w(:)
    character(len=*), intent(inOUT) :: x(:)
    character(len=*), intent(OUT) :: y(:)
    character(len=12) :: foostring(2) = (/"0123456789#$" , &
                                          "$#9876543210"/)
! This next is not required by the standard but tests the
! functioning of the gfortran implementation.
!   if (all (x(:)(3:7) .eq. y)) call abort ()
    x = foostring (:)(5 : 4 + len (x))
    y = foostring (:)(3 : 2 + len (y))
  end subroutine foo
end program pr28174

