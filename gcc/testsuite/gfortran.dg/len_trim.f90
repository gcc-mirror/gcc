! { dg-do compile }
! { dg-options "-O -Wall -Wconversion-extra -fdump-tree-original" }
! { dg-final { scan-tree-dump-not "_gfortran_stop_numeric" "original" } }
! PR fortran/87711 - ICE in gfc_trans_transfer
! PR fortran/87851 - return type for len_trim

program main
  implicit none
  character(3), parameter :: a(1) = 'aa'
  character(3)            :: b    = "bb"
  character(3)            :: c(1) = 'cc'
  integer(4), parameter   :: l4(1) = len_trim (a, kind=4)
  integer(8), parameter   :: l8(1) = len_trim (a, kind=8)
  integer                 :: kk(1) = len_trim (a)
  integer(4)              :: mm(1) = len_trim (a, kind=4)
  integer(8)              :: nn(1) = len_trim (a, kind=8)
  kk = len_trim (a)
  mm = len_trim (a, kind=4)
  nn = len_trim (a, kind=8)
  mm = len_trim (a,      4)
  nn = len_trim (a,      8)
  kk = len_trim ([b])
  mm = len_trim ([b],kind=4)
  nn = len_trim ([b],kind=8)
  mm = len_trim ([b],     4)
  nn = len_trim ([b],     8)
  kk = len_trim (c)
  mm = len_trim (c, kind=4)
  nn = len_trim (c, kind=8)
  mm = len_trim (c,      4)
  nn = len_trim (c,      8)
  if (any (l4 /= 2_4) .or. any (l8 /= 2_8)) stop 1
end program main
