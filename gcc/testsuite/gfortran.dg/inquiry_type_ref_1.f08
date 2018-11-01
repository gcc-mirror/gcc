! { dg-do run }
!
! Test the implementation of inquiry part references (PR40196).
! "Type parameter inquiry (str%len, a%kind) and Complex parts (z%re, z%im)"
!
! Contributed by Tobias Burnus  <burnus@gcc.gnu.org>
!
module m
  complex, target :: z
  character (:), allocatable :: str
  real, pointer :: r => z%re
  real, pointer :: i => z%im
  type :: mytype
    complex :: z = ( 10.0, 11.0 )
    character(6) :: str
  end type
end module

  use m

  type(mytype) :: der
  integer :: j
  character (len=der%str%len) :: str1
  complex, parameter :: zc = ( 99.0, 199.0 )
  REAL, parameter :: rc = zc%re
  REAL, parameter :: ic = zc%im

  z = (2.0,4.0)
  str = "abcd"

! Check the pointer initializations
  if (r .ne. real (z)) stop 1
  if (i .ne. imag (z)) stop 2

! Check the use of inquiry part_refs on lvalues and rvalues.
  z%im = 4.0 * z%re

! Check that the result is OK.
  if (z%re .ne. real (z)) stop 3
  if (abs (z*im - 4.0 * real (z)) .lt. 1e-6) stop 4

! Check a double inquiry part_ref.
  if (z%im%kind .ne. kind (z)) stop 5

! Test on deferred character length.
  if (str%kind .ne. kind (str)) stop 6
  if (str%len .ne. len (str)) stop 7

! Check the use in specification expressions.
  if (len (der%str) .ne. LEN (str1)) stop 8
  if (rc .ne. real (zc)) stop 9
  if (ic .ne. aimag (zc)) stop 10

end

