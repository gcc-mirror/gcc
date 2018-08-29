! { dg-do run }
! PR35937, in which letting the length of 'c' to kind = 8 would
! screw up the interface and would cause an ICE. Note that this is
! actually the example of comment #4.
!
! Contributed by Thomas Koenig <tkoenig@gcc.gnu.org>
!
program main
  implicit none
  if (f5 ('1') .ne. "a") STOP 1
  if (len (f5 ('1')) .ne. 1) STOP 1
  if (f5 ('4') .ne. "abcd") STOP 1
  if (len (f5 ('4')) .ne. 4) STOP 1
contains
  function f5 (c)
    character(len=1_8) :: c
    character(len=scan('123456789', c)) :: f5
    integer :: i
    do i = 1, len (f5)
       f5(i:i) = char (i+96)
    end do
  end function f5
end program main
