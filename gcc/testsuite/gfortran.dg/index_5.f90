! { dg-do compile }
!
! PR fortran/97896
! An ICE occured with INDEX when the KIND argument was present
! because of a mismatch between the number of arguments expected
! during the scalarization process and the number of arguments actually
! used.
!
! Test contributed by Harald Anlauf <anlauf@gcc.gnu.org>, based on an initial
! submission by G. Steinmetz <gscfq@t-online.de>.

program p
  implicit none
  logical    :: a(2)
  integer    :: b(2)
  integer(8) :: d(2)
  b = index ('xyxyz','yx', back=a)
  b = index ('xyxyz','yx', back=a, kind=4)
  d = index ('xyxyz','yx', back=a, kind=8)
  b = index ('xyxyz','yx', back=a, kind=8)
  d = index ('xyxyz','yx', back=a, kind=4)
  b = index ('xyxyz','yx',      a,      4)
  d = index ('xyxyz','yx',      a,      8)
end

