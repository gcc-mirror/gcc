! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }
!
! PR fortran/101735 - substrings and parsing of type parameter inquiries

program p
  implicit none
  integer, parameter :: ck = 4
  character(len=5)         :: str  = ""
  character(len=5)         :: str2(4)
  character(len=5,kind=ck) :: str4 = ck_""
  type t
     character(len=5) :: str(4)
  end type t
  type(t) :: var
  integer :: x, y 

  integer, parameter :: i1 = kind (str(1:3))
  integer, parameter :: j1 = str (1:3) % kind
  integer, parameter :: k1 = (str(1:3) % kind)
  integer, parameter :: kk = str (1:3) % kind % kind

  integer, parameter :: i4 = kind (str4(1:3))
  integer, parameter :: j4 = str4 (1:3) % kind
  integer, parameter :: ll = str4 (1:3) % len

  integer, parameter :: i2 = len (str(1:3))
  integer, parameter :: j2 = str (1:3) % len
  integer, parameter :: k2 = (str(1:3) % len)
  integer, parameter :: lk = str (1:3) % len  % kind

  integer, parameter :: l4 = str2      (:) (2:3) % len
  integer, parameter :: l5 = var % str (:) (2:4) % len
  integer, parameter :: k4 = str2      (:) (2:3) % kind
  integer, parameter :: k5 = var % str (:) (2:4) % kind
  integer, parameter :: k6 = str2      (:) (2:3) % len % kind
  integer, parameter :: k7 = var % str (:) (2:4) % len % kind

  if (i1 /= 1) stop 1
  if (j1 /= 1) stop 2
  if (k1 /= 1) stop 3

  if (i4 /= ck) stop 4
  if (j4 /= ck) stop 5
  if (ll /= 3)  stop 6

  if (kk /= 4) stop 7
  if (lk /= 4) stop 8

  if (i2 /= 3) stop 9
  if (j2 /= 3) stop 10
  if (k2 /= 3) stop 11

  if (l4 /= 2) stop 12
  if (l5 /= 3) stop 13
  if (k4 /= 1) stop 14
  if (k5 /= 1) stop 15
  if (k6 /= 4) stop 16
  if (k7 /= 4) stop 17
end

! { dg-final { scan-tree-dump-not "_gfortran_stop_numeric" "original" } }
