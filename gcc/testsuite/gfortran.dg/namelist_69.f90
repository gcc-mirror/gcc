! { dg-do run }
!
! PR fortran/47339
! PR fortran/43062
!
! Run-time test for Fortran 2003 NAMELISTS
! Version for non-strings
!
program nml_test
  implicit none

  character(len=1000) :: str

  integer, allocatable :: a(:)
  integer, allocatable :: b
  integer, pointer :: ap(:)
  integer, pointer :: bp
  integer :: c
  integer :: d(3)

  type t
    integer :: c1
    integer :: c2(3)
  end type t
  type(t) :: e,f(2)
  type(t),allocatable :: g,h(:)
  type(t),pointer :: i,j(:)

  namelist /nml/ a, b, c, d, ap, bp,e,f,g,h,i,j

  a = [1,2]
  allocate(b,ap(2),bp)
  ap = [98, 99]
  b = 7
  bp = 101
  c = 8
  d = [-1, -2, -3]

  e%c1 = -701
  e%c2 = [-702,-703,-704]
  f(1)%c1 = 33001
  f(2)%c1 = 33002
  f(1)%c2 = [44001,44002,44003]
  f(2)%c2 = [44011,44012,44013]

  allocate(g,h(2),i,j(2))

  g%c1 = -601
  g%c2 = [-602,6703,-604]
  h(1)%c1 = 35001
  h(2)%c1 = 35002
  h(1)%c2 = [45001,45002,45003]
  h(2)%c2 = [45011,45012,45013]

  i%c1 = -501
  i%c2 = [-502,-503,-504]
  j(1)%c1 = 36001
  j(2)%c1 = 36002
  j(1)%c2 = [46001,46002,46003]
  j(2)%c2 = [46011,46012,46013]

  ! SAVE NAMELIST
  str = repeat('X', len(str))
  write(str,nml=nml)

  ! RESET NAMELIST
  a = [-1,-1]
  ap = [-1, -1]
  b = -1
  bp = -1
  c = -1
  d = [-1, -1, -1]

  e%c1 = -1
  e%c2 = [-1,-1,-1]
  f(1)%c1 = -1
  f(2)%c1 = -1
  f(1)%c2 = [-1,-1,-1]
  f(2)%c2 = [-1,-1,-1]

  g%c1 = -1
  g%c2 = [-1,-1,-1]
  h(1)%c1 = -1
  h(2)%c1 = -1
  h(1)%c2 = [-1,-1,-1]
  h(2)%c2 = [-1,-1,-1]

  i%c1 = -1
  i%c2 = [-1,-1,-1]
  j(1)%c1 = -1
  j(2)%c1 = -1
  j(1)%c2 = [-1,-1,-1]
  j(2)%c2 = [-1,-1,-1]

  ! Read back
  read(str,nml=nml)

  ! Check result
  if (any (a /= [1,2])) STOP 1
  if (any (ap /= [98, 99])) STOP 2
  if (b /= 7) STOP 3
  if (bp /= 101) STOP 4
  if (c /= 8) STOP 5
  if (any (d /= [-1, -2, -3])) STOP 6

  if (e%c1 /= -701) STOP 7
  if (any (e%c2 /= [-702,-703,-704])) STOP 8
  if (f(1)%c1 /= 33001) STOP 9
  if (f(2)%c1 /= 33002) STOP 10
  if (any (f(1)%c2 /= [44001,44002,44003])) STOP 11
  if (any (f(2)%c2 /= [44011,44012,44013])) STOP 12

  if (g%c1 /= -601) STOP 13
  if (any(g%c2 /= [-602,6703,-604])) STOP 14
  if (h(1)%c1 /= 35001) STOP 15
  if (h(2)%c1 /= 35002) STOP 16
  if (any (h(1)%c2 /= [45001,45002,45003])) STOP 17
  if (any (h(2)%c2 /= [45011,45012,45013])) STOP 18

  if (i%c1 /= -501) STOP 19
  if (any (i%c2 /= [-502,-503,-504])) STOP 20
  if (j(1)%c1 /= 36001) STOP 21
  if (j(2)%c1 /= 36002) STOP 22
  if (any (j(1)%c2 /= [46001,46002,46003])) STOP 23
  if (any (j(2)%c2 /= [46011,46012,46013])) STOP 24

  ! Check argument passing (dummy processing)
  call test2(a,b,c,d,ap,bp,e,f,g,h,i,j,2) 

contains
  subroutine test2(x1,x2,x3,x4,x1p,x2p,x6,x7,x8,x9,x10,x11,n)
    integer, allocatable :: x1(:)
    integer, allocatable :: x2
    integer, pointer :: x1p(:)
    integer, pointer :: x2p
    integer :: x3
    integer :: x4(3)
    integer :: n
    integer :: x5(n)
    type(t) :: x6,x7(2)
    type(t),allocatable :: x8,x9(:)
    type(t),pointer :: x10,x11(:)
    type(t) :: x12(n)

    namelist /nml2/ x1, x2, x3, x4,x5,x1p,x2p,x6,x7,x8,x9,x10,x11,x12

    x5 = [ 42, 53 ]

    x12(1)%c1 = 37001
    x12(2)%c1 = 37002
    x12(1)%c2 = [47001,47002,47003]
    x12(2)%c2 = [47011,47012,47013]

    ! SAVE NAMELIST
    str = repeat('X', len(str))
    write(str,nml=nml2)

    ! RESET NAMELIST
    x1 = [-1,-1]
    x1p = [-1, -1]
    x2 = -1
    x2p = -1
    x3 = -1
    x4 = [-1, -1, -1]

    x6%c1 = -1
    x6%c2 = [-1,-1,-1]
    x7(1)%c1 = -1
    x7(2)%c1 = -1
    x7(1)%c2 = [-1,-1,-1]
    x7(2)%c2 = [-1,-1,-1]

    x8%c1 = -1
    x8%c2 = [-1,-1,-1]
    x9(1)%c1 = -1
    x9(2)%c1 = -1
    x9(1)%c2 = [-1,-1,-1]
    x9(2)%c2 = [-1,-1,-1]

    x10%c1 = -1
    x10%c2 = [-1,-1,-1]
    x11(1)%c1 = -1
    x11(2)%c1 = -1
    x11(1)%c2 = [-1,-1,-1]
    x11(2)%c2 = [-1,-1,-1]

    x5 = [ -1, -1 ]

    x12(1)%c1 = -1
    x12(2)%c1 = -1
    x12(1)%c2 = [-1,-1,-1]
    x12(2)%c2 = [-1,-1,-1]

    ! Read back
    read(str,nml=nml2)

    ! Check result
    if (any (x1 /= [1,2])) STOP 25
    if (any (x1p /= [98, 99])) STOP 26
    if (x2 /= 7) STOP 27
    if (x2p /= 101) STOP 28
    if (x3 /= 8) STOP 29
    if (any (x4 /= [-1, -2, -3])) STOP 30

    if (x6%c1 /= -701) STOP 31
    if (any (x6%c2 /= [-702,-703,-704])) STOP 32
    if (x7(1)%c1 /= 33001) STOP 33
    if (x7(2)%c1 /= 33002) STOP 34
    if (any (x7(1)%c2 /= [44001,44002,44003])) STOP 35
    if (any (x7(2)%c2 /= [44011,44012,44013])) STOP 36

    if (x8%c1 /= -601) STOP 37
    if (any(x8%c2 /= [-602,6703,-604])) STOP 38
    if (x9(1)%c1 /= 35001) STOP 39
    if (x9(2)%c1 /= 35002) STOP 40
    if (any (x9(1)%c2 /= [45001,45002,45003])) STOP 41
    if (any (x9(2)%c2 /= [45011,45012,45013])) STOP 42

    if (x10%c1 /= -501) STOP 43
    if (any (x10%c2 /= [-502,-503,-504])) STOP 44
    if (x11(1)%c1 /= 36001) STOP 45
    if (x11(2)%c1 /= 36002) STOP 46
    if (any (x11(1)%c2 /= [46001,46002,46003])) STOP 47
    if (any (x11(2)%c2 /= [46011,46012,46013])) STOP 48

    if (any (x5 /= [ 42, 53 ])) STOP 49

    if (x12(1)%c1 /= 37001) STOP 50
    if (x12(2)%c1 /= 37002) STOP 51
    if (any (x12(1)%c2 /= [47001,47002,47003])) STOP 52
    if (any (x12(2)%c2 /= [47011,47012,47013])) STOP 53
  end subroutine test2
end program nml_test
