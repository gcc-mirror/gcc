! { dg-do run }
!
! CO_BROADCAST
!
program test
  implicit none
  intrinsic co_broadcast

  type t
    integer :: i
    character(len=1) :: c
    real(8) :: x(3), y(3)
  end type t

  integer :: i, j(10), stat
  complex :: a(5,5)
  character(kind=1, len=5) :: str1, errstr
  character(kind=4, len=8) :: str2(2)
  type(t) :: dt(4)

  i = 1
  j = 55
  a = 99.0
  str1 = 1_"XXXXX"
  str2 = 4_"YYYYYYYY"
  dt = t(1, 'C', [1.,2.,3.], [3,3,3])
  errstr = "ZZZZZ"

  if (this_image() == num_images()) then
    i = 2
    j = 66
    a = -99.0
    str1 = 1_"abcd"
    str2 = 4_"12 3 4 5"
    dt = t(-1, 'a', [3.,1.,8.], [99,24,5])
  end if
  sync all

  call co_broadcast(i, source_image=num_images(), stat=stat, errmsg=errstr)
  if (stat /= 0) STOP 1
  if (errstr /= "ZZZZZ") STOP 2
  if (i /= 2) STOP 3

  call co_broadcast(j, source_image=num_images(), stat=stat, errmsg=errstr)
  if (stat /= 0) STOP 4
  if (errstr /= "ZZZZZ") STOP 5
  if (any (j /= 66)) STOP 1

  call co_broadcast(a, source_image=num_images(), stat=stat, errmsg=errstr)
  if (stat /= 0) STOP 6
  if (errstr /= "ZZZZZ") STOP 7
  if (any (a /= -99.0)) STOP 2

  call co_broadcast(str1, source_image=num_images(), stat=stat, errmsg=errstr)
  if (stat /= 0) STOP 8
  if (errstr /= "ZZZZZ") STOP 9
  if (str1 /= "abcd") STOP 10

  call co_broadcast(str2, source_image=num_images(), stat=stat, errmsg=errstr)
  if (stat /= 0) STOP 11
  if (errstr /= "ZZZZZ") STOP 12
  if (any (str2 /= 4_"12 3 4 5")) STOP 3

  call co_broadcast(dt, source_image=num_images(), stat=stat, errmsg=errstr)
  if (stat /= 0) STOP 13
  if (errstr /= "ZZZZZ") STOP 14
  if (any (dt(:)%i /= -1)) STOP 15
  if (any (dt(:)%c /= 'a')) STOP 16
  if (any (dt(:)%x(1) /= 3.)) STOP 17
  if (any (dt(:)%x(2) /= 1.)) STOP 18
  if (any (dt(:)%x(3) /= 8.)) STOP 19
  if (any (dt(:)%y(1) /= 99.)) STOP 20
  if (any (dt(:)%y(2) /= 24.)) STOP 21
  if (any (dt(:)%y(3) /= 5.)) STOP 22

  sync all
  dt = t(1, 'C', [1.,2.,3.], [3,3,3])
  sync all
  if (this_image() == num_images()) then
    str2 = 4_"001122"
    dt(2:4) = t(-2, 'i', [9.,2.,3.], [4,44,321])
  end if

  call co_broadcast(str2(::2), source_image=num_images(), stat=stat, &
                    errmsg=errstr)
  if (stat /= 0) STOP 23
  if (errstr /= "ZZZZZ") STOP 24
  if (str2(1) /= 4_"001122") STOP 25
  if (this_image() == num_images()) then
    if (str2(1) /= 4_"001122") STOP 26
  else
    if (str2(2) /= 4_"12 3 4 5") STOP 27
  end if

  call co_broadcast(dt(2::2), source_image=num_images(), stat=stat, &
                    errmsg=errstr)
  if (stat /= 0) STOP 28
  if (errstr /= "ZZZZZ") STOP 29
  if (this_image() == num_images()) then
    if (any (dt(1:1)%i /= 1)) STOP 30
    if (any (dt(1:1)%c /= 'C')) STOP 31
    if (any (dt(1:1)%x(1) /= 1.)) STOP 32
    if (any (dt(1:1)%x(2) /= 2.)) STOP 33
    if (any (dt(1:1)%x(3) /= 3.)) STOP 34
    if (any (dt(1:1)%y(1) /= 3.)) STOP 35
    if (any (dt(1:1)%y(2) /= 3.)) STOP 36
    if (any (dt(1:1)%y(3) /= 3.)) STOP 37

    if (any (dt(2:)%i /= -2)) STOP 38
    if (any (dt(2:)%c /= 'i')) STOP 39
    if (any (dt(2:)%x(1) /= 9.)) STOP 40
    if (any (dt(2:)%x(2) /= 2.)) STOP 41
    if (any (dt(2:)%x(3) /= 3.)) STOP 42
    if (any (dt(2:)%y(1) /= 4.)) STOP 43
    if (any (dt(2:)%y(2) /= 44.)) STOP 44
    if (any (dt(2:)%y(3) /= 321.)) STOP 45
  else
    if (any (dt(1::2)%i /= 1)) STOP 46
    if (any (dt(1::2)%c /= 'C')) STOP 47
    if (any (dt(1::2)%x(1) /= 1.)) STOP 48
    if (any (dt(1::2)%x(2) /= 2.)) STOP 49
    if (any (dt(1::2)%x(3) /= 3.)) STOP 50
    if (any (dt(1::2)%y(1) /= 3.)) STOP 51
    if (any (dt(1::2)%y(2) /= 3.)) STOP 52
    if (any (dt(1::2)%y(3) /= 3.)) STOP 53

    if (any (dt(2::2)%i /= -2)) STOP 54
    if (any (dt(2::2)%c /= 'i')) STOP 55
    if (any (dt(2::2)%x(1) /= 9.)) STOP 56
    if (any (dt(2::2)%x(2) /= 2.)) STOP 57
    if (any (dt(2::2)%x(3) /= 3.)) STOP 58
    if (any (dt(2::2)%y(1) /= 4.)) STOP 59
    if (any (dt(2::2)%y(2) /= 44.)) STOP 60
    if (any (dt(2::2)%y(3) /= 321.)) STOP 61
  endif
end program test
