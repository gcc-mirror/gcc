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

  character(len=5), allocatable :: a(:)
  character(len=5), allocatable :: b
  character(len=5), pointer :: ap(:)
  character(len=5), pointer :: bp
  character(len=5) :: c
  character(len=5) :: d(3)

  type t
    character(len=5) :: c1
    character(len=5) :: c2(3)
  end type t
  type(t) :: e,f(2)
  type(t),allocatable :: g,h(:)
  type(t),pointer :: i,j(:)

  namelist /nml/ a, b, c, d, ap, bp,e,f,g,h,i,j

  a = ["aa01", "aa02"]
  allocate(b,ap(2),bp)
  ap = ['98', '99']
  b = '7'
  bp = '101'
  c = '8'
  d = ['-1', '-2', '-3']

  e%c1 = '-701'
  e%c2 = ['-702','-703','-704']
  f(1)%c1 = '33001'
  f(2)%c1 = '33002'
  f(1)%c2 = ['44001','44002','44003']
  f(2)%c2 = ['44011','44012','44013']

  allocate(g,h(2),i,j(2))

  g%c1 = '-601'
  g%c2 = ['-602','6703','-604']
  h(1)%c1 = '35001'
  h(2)%c1 = '35002'
  h(1)%c2 = ['45001','45002','45003']
  h(2)%c2 = ['45011','45012','45013']

  i%c1 = '-501'
  i%c2 = ['-502','-503','-504']
  j(1)%c1 = '36001'
  j(2)%c1 = '36002'
  j(1)%c2 = ['46001','46002','46003']
  j(2)%c2 = ['46011','46012','46013']

  ! SAVE NAMELIST
  str = repeat('X', len(str))
  write(str,nml=nml)

  ! RESET NAMELIST
  a = repeat('X', len(a))
  ap = repeat('X', len(ap))
  b = repeat('X', len(b))
  bp = repeat('X', len(bp))
  c = repeat('X', len(c))
  d = repeat('X', len(d))

  e%c1 = repeat('X', len(e%c1))
  e%c2 = repeat('X', len(e%c2))
  f(1)%c1 = repeat('X', len(f(1)%c1))
  f(2)%c1 = repeat('X', len(f(2)%c1))
  f(1)%c2 = repeat('X', len(f(1)%c2))
  f(2)%c2 = repeat('X', len(f(2)%c2))

  g%c1 = repeat('X', len(g%c1))
  g%c2 = repeat('X', len(g%c1))
  h(1)%c1 = repeat('X', len(h(1)%c1))
  h(2)%c1 = repeat('X', len(h(1)%c1))
  h(1)%c2 = repeat('X', len(h(1)%c1))
  h(2)%c2 = repeat('X', len(h(1)%c1))

  i%c1 = repeat('X', len(i%c1))
  i%c2 = repeat('X', len(i%c1))
  j(1)%c1 = repeat('X', len(j(1)%c1))
  j(2)%c1 = repeat('X', len(j(2)%c1))
  j(1)%c2 = repeat('X', len(j(1)%c2))
  j(2)%c2 = repeat('X', len(j(2)%c2))

  ! Read back
  read(str,nml=nml)

  ! Check result
  if (any (a /= ['aa01','aa02'])) STOP 1
  if (any (ap /= ['98', '99'])) STOP 2
  if (b /= '7') STOP 3
  if (bp /= '101') STOP 4
  if (c /= '8') STOP 5
  if (any (d /= ['-1', '-2', '-3'])) STOP 6

  if (e%c1 /= '-701') STOP 7
  if (any (e%c2 /= ['-702','-703','-704'])) STOP 8
  if (f(1)%c1 /= '33001') STOP 9
  if (f(2)%c1 /= '33002') STOP 10
  if (any (f(1)%c2 /= ['44001','44002','44003'])) STOP 11
  if (any (f(2)%c2 /= ['44011','44012','44013'])) STOP 12

  if (g%c1 /= '-601') STOP 13
  if (any(g%c2 /= ['-602','6703','-604'])) STOP 14
  if (h(1)%c1 /= '35001') STOP 15
  if (h(2)%c1 /= '35002') STOP 16
  if (any (h(1)%c2 /= ['45001','45002','45003'])) STOP 17
  if (any (h(2)%c2 /= ['45011','45012','45013'])) STOP 18

  if (i%c1 /= '-501') STOP 19
  if (any (i%c2 /= ['-502','-503','-504'])) STOP 20
  if (j(1)%c1 /= '36001') STOP 21
  if (j(2)%c1 /= '36002') STOP 22
  if (any (j(1)%c2 /= ['46001','46002','46003'])) STOP 23
  if (any (j(2)%c2 /= ['46011','46012','46013'])) STOP 24

  ! Check argument passing (dummy processing)
  call test2(a,b,c,d,ap,bp,e,f,g,h,i,j,2) 
  call test3(a,b,c,d,ap,bp,e,f,g,h,i,j,2,len(a)) 
  call test4(a,b,c,d,ap,bp,e,f,g,h,i,j,2)

contains
  subroutine test2(x1,x2,x3,x4,x1p,x2p,x6,x7,x8,x9,x10,x11,n)
    character(len=5), allocatable :: x1(:)
    character(len=5), allocatable :: x2
    character(len=5), pointer :: x1p(:)
    character(len=5), pointer :: x2p
    character(len=5) :: x3
    character(len=5) :: x4(3)
    integer :: n
    character(len=5) :: x5(n)
    type(t) :: x6,x7(2)
    type(t),allocatable :: x8,x9(:)
    type(t),pointer :: x10,x11(:)
    type(t) :: x12(n)

    namelist /nml2/ x1, x2, x3, x4,x5,x1p,x2p,x6,x7,x8,x9,x10,x11,x12

    x5 = [ 'x5-42', 'x5-53' ]

    x12(1)%c1 = '37001'
    x12(2)%c1 = '37002'
    x12(1)%c2 = ['47001','47002','47003']
    x12(2)%c2 = ['47011','47012','47013']
 
    ! SAVE NAMELIST
    str = repeat('X', len(str))
    write(str,nml=nml2)

    ! RESET NAMELIST
    x1 = repeat('X', len(x1))
    x1p = repeat('X', len(x1p))
    x2 = repeat('X', len(x2))
    x2p = repeat('X', len(x2p))
    x3 = repeat('X', len(x3))
    x4 = repeat('X', len(x4))

    x6%c1 = repeat('X', len(x6%c1))
    x6%c2 = repeat('X', len(x6%c2))
    x7(1)%c1 = repeat('X', len(x7(1)%c1))
    x7(2)%c1 = repeat('X', len(x7(2)%c1))
    x7(1)%c2 = repeat('X', len(x7(1)%c2))
    x7(2)%c2 = repeat('X', len(x7(2)%c2))

    x8%c1 = repeat('X', len(x8%c1))
    x8%c2 = repeat('X', len(x8%c1))
    x9(1)%c1 = repeat('X', len(x9(1)%c1))
    x9(2)%c1 = repeat('X', len(x9(1)%c1))
    x9(1)%c2 = repeat('X', len(x9(1)%c1))
    x9(2)%c2 = repeat('X', len(x9(1)%c1))

    x10%c1 = repeat('X', len(x10%c1))
    x10%c2 = repeat('X', len(x10%c1))
    x11(1)%c1 = repeat('X', len(x11(1)%c1))
    x11(2)%c1 = repeat('X', len(x11(2)%c1))
    x11(1)%c2 = repeat('X', len(x11(1)%c2))
    x11(2)%c2 = repeat('X', len(x11(2)%c2))

    x5 = repeat('X', len(x5))

    x12(1)%c1 = repeat('X', len(x12(2)%c2))
    x12(2)%c1 = repeat('X', len(x12(2)%c2))
    x12(1)%c2 = repeat('X', len(x12(2)%c2))
    x12(2)%c2 = repeat('X', len(x12(2)%c2))

    ! Read back
    read(str,nml=nml2)

    ! Check result
    if (any (x1 /= ['aa01','aa02'])) STOP 25
    if (any (x1p /= ['98', '99'])) STOP 26
    if (x2 /= '7') STOP 27
    if (x2p /= '101') STOP 28
    if (x3 /= '8') STOP 29
    if (any (x4 /= ['-1', '-2', '-3'])) STOP 30

    if (x6%c1 /= '-701') STOP 31
    if (any (x6%c2 /= ['-702','-703','-704'])) STOP 32
    if (x7(1)%c1 /= '33001') STOP 33
    if (x7(2)%c1 /= '33002') STOP 34
    if (any (x7(1)%c2 /= ['44001','44002','44003'])) STOP 35
    if (any (x7(2)%c2 /= ['44011','44012','44013'])) STOP 36

    if (x8%c1 /= '-601') STOP 37
    if (any(x8%c2 /= ['-602','6703','-604'])) STOP 38
    if (x9(1)%c1 /= '35001') STOP 39
    if (x9(2)%c1 /= '35002') STOP 40
    if (any (x9(1)%c2 /= ['45001','45002','45003'])) STOP 41
    if (any (x9(2)%c2 /= ['45011','45012','45013'])) STOP 42
 
    if (x10%c1 /= '-501') STOP 43
    if (any (x10%c2 /= ['-502','-503','-504'])) STOP 44
    if (x11(1)%c1 /= '36001') STOP 45
    if (x11(2)%c1 /= '36002') STOP 46
    if (any (x11(1)%c2 /= ['46001','46002','46003'])) STOP 47
    if (any (x11(2)%c2 /= ['46011','46012','46013'])) STOP 48

    if (any (x5 /= [ 'x5-42', 'x5-53' ])) STOP 49

    if (x12(1)%c1 /= '37001') STOP 50
    if (x12(2)%c1 /= '37002') STOP 51
    if (any (x12(1)%c2 /= ['47001','47002','47003'])) STOP 52
    if (any (x12(2)%c2 /= ['47011','47012','47013'])) STOP 53
  end subroutine test2

  subroutine test3(x1,x2,x3,x4,x1p,x2p,x6,x7,x8,x9,x10,x11,n,ll)
    integer :: n, ll
    character(len=ll), allocatable :: x1(:)
    character(len=ll), allocatable :: x2
    character(len=ll), pointer :: x1p(:)
    character(len=ll), pointer :: x2p
    character(len=ll) :: x3
    character(len=ll) :: x4(3)
    character(len=ll) :: x5(n)
    type(t) :: x6,x7(2)
    type(t),allocatable :: x8,x9(:)
    type(t),pointer :: x10,x11(:)
    type(t) :: x12(n)

   namelist /nml2/ x1, x2, x3, x4,x5,x1p,x2p,x6,x7,x8,x9,x10,x11,x12

    x5 = [ 'x5-42', 'x5-53' ]

    x12(1)%c1 = '37001'
    x12(2)%c1 = '37002'
    x12(1)%c2 = ['47001','47002','47003']
    x12(2)%c2 = ['47011','47012','47013']
 
    ! SAVE NAMELIST
    str = repeat('X', len(str))
    write(str,nml=nml2)

    ! RESET NAMELIST
    x1 = repeat('X', len(x1))
    x1p = repeat('X', len(x1p))

    x2 = repeat('X', len(x2))
    x2p = repeat('X', len(x2p))
    x3 = repeat('X', len(x3))
    x4 = repeat('X', len(x4))

    x6%c1 = repeat('X', len(x6%c1))
    x6%c2 = repeat('X', len(x6%c2))
    x7(1)%c1 = repeat('X', len(x7(1)%c1))
    x7(2)%c1 = repeat('X', len(x7(2)%c1))
    x7(1)%c2 = repeat('X', len(x7(1)%c2))
    x7(2)%c2 = repeat('X', len(x7(2)%c2))

    x8%c1 = repeat('X', len(x8%c1))
    x8%c2 = repeat('X', len(x8%c1))
    x9(1)%c1 = repeat('X', len(x9(1)%c1))
    x9(2)%c1 = repeat('X', len(x9(1)%c1))
    x9(1)%c2 = repeat('X', len(x9(1)%c1))
    x9(2)%c2 = repeat('X', len(x9(1)%c1))

    x10%c1 = repeat('X', len(x10%c1))
    x10%c2 = repeat('X', len(x10%c1))
    x11(1)%c1 = repeat('X', len(x11(1)%c1))
    x11(2)%c1 = repeat('X', len(x11(2)%c1))
    x11(1)%c2 = repeat('X', len(x11(1)%c2))
    x11(2)%c2 = repeat('X', len(x11(2)%c2))

    x5 = repeat('X', len(x5))

    x12(1)%c1 = repeat('X', len(x12(2)%c2))
    x12(2)%c1 = repeat('X', len(x12(2)%c2))
    x12(1)%c2 = repeat('X', len(x12(2)%c2))
    x12(2)%c2 = repeat('X', len(x12(2)%c2))

    ! Read back
    read(str,nml=nml2)

    ! Check result
    if (any (x1 /= ['aa01','aa02'])) STOP 54
    if (any (x1p /= ['98', '99'])) STOP 55
    if (x2 /= '7') STOP 56
    if (x2p /= '101') STOP 57
    if (x3 /= '8') STOP 58
    if (any (x4 /= ['-1', '-2', '-3'])) STOP 59

    if (x6%c1 /= '-701') STOP 60
    if (any (x6%c2 /= ['-702','-703','-704'])) STOP 61
    if (x7(1)%c1 /= '33001') STOP 62
    if (x7(2)%c1 /= '33002') STOP 63
    if (any (x7(1)%c2 /= ['44001','44002','44003'])) STOP 64
    if (any (x7(2)%c2 /= ['44011','44012','44013'])) STOP 65

    if (x8%c1 /= '-601') STOP 66
    if (any(x8%c2 /= ['-602','6703','-604'])) STOP 67
    if (x9(1)%c1 /= '35001') STOP 68
    if (x9(2)%c1 /= '35002') STOP 69
    if (any (x9(1)%c2 /= ['45001','45002','45003'])) STOP 70
    if (any (x9(2)%c2 /= ['45011','45012','45013'])) STOP 71
 
    if (x10%c1 /= '-501') STOP 72
    if (any (x10%c2 /= ['-502','-503','-504'])) STOP 73
    if (x11(1)%c1 /= '36001') STOP 74
    if (x11(2)%c1 /= '36002') STOP 75
    if (any (x11(1)%c2 /= ['46001','46002','46003'])) STOP 76
    if (any (x11(2)%c2 /= ['46011','46012','46013'])) STOP 77

    if (any (x5 /= [ 'x5-42', 'x5-53' ])) STOP 78

    if (x12(1)%c1 /= '37001') STOP 79
    if (x12(2)%c1 /= '37002') STOP 80
    if (any (x12(1)%c2 /= ['47001','47002','47003'])) STOP 81
    if (any (x12(2)%c2 /= ['47011','47012','47013'])) STOP 82
  end subroutine test3

  subroutine test4(x1,x2,x3,x4,x1p,x2p,x6,x7,x8,x9,x10,x11,n)
    character(len=*), allocatable :: x1(:)
    character(len=*), allocatable :: x2
    character(len=*), pointer :: x1p(:)
    character(len=*), pointer :: x2p
    character(len=*) :: x3
    character(len=*) :: x4(3)
    integer :: n
    character(len=5) :: x5(n)
    type(t) :: x6,x7(2)
    type(t),allocatable :: x8,x9(:)
    type(t),pointer :: x10,x11(:)
    type(t) :: x12(n)

    namelist /nml2/ x1, x2, x3, x4,x5,x1p,x2p,x6,x7,x8,x9,x10,x11,x12

    x5 = [ 'x5-42', 'x5-53' ]

    x12(1)%c1 = '37001'
    x12(2)%c1 = '37002'
    x12(1)%c2 = ['47001','47002','47003']
    x12(2)%c2 = ['47011','47012','47013']
 
    ! SAVE NAMELIST
    str = repeat('X', len(str))
    write(str,nml=nml2)

    ! RESET NAMELIST
    x1 = repeat('X', len(x1))
    x1p = repeat('X', len(x1p))
    x2 = repeat('X', len(x2))
    x2p = repeat('X', len(x2p))
    x3 = repeat('X', len(x3))
    x4 = repeat('X', len(x4))

    x6%c1 = repeat('X', len(x6%c1))
    x6%c2 = repeat('X', len(x6%c2))
    x7(1)%c1 = repeat('X', len(x7(1)%c1))
    x7(2)%c1 = repeat('X', len(x7(2)%c1))
    x7(1)%c2 = repeat('X', len(x7(1)%c2))
    x7(2)%c2 = repeat('X', len(x7(2)%c2))

    x8%c1 = repeat('X', len(x8%c1))
    x8%c2 = repeat('X', len(x8%c1))
    x9(1)%c1 = repeat('X', len(x9(1)%c1))
    x9(2)%c1 = repeat('X', len(x9(1)%c1))
    x9(1)%c2 = repeat('X', len(x9(1)%c1))
    x9(2)%c2 = repeat('X', len(x9(1)%c1))

    x10%c1 = repeat('X', len(x10%c1))
    x10%c2 = repeat('X', len(x10%c1))
    x11(1)%c1 = repeat('X', len(x11(1)%c1))
    x11(2)%c1 = repeat('X', len(x11(2)%c1))
    x11(1)%c2 = repeat('X', len(x11(1)%c2))
    x11(2)%c2 = repeat('X', len(x11(2)%c2))

    x5 = repeat('X', len(x5))

    x12(1)%c1 = repeat('X', len(x12(2)%c2))
    x12(2)%c1 = repeat('X', len(x12(2)%c2))
    x12(1)%c2 = repeat('X', len(x12(2)%c2))
    x12(2)%c2 = repeat('X', len(x12(2)%c2))

    ! Read back
    read(str,nml=nml2)

    ! Check result
    if (any (x1 /= ['aa01','aa02'])) STOP 83
    if (any (x1p /= ['98', '99'])) STOP 84
    if (x2 /= '7') STOP 85
    if (x2p /= '101') STOP 86
    if (x3 /= '8') STOP 87
    if (any (x4 /= ['-1', '-2', '-3'])) STOP 88

    if (x6%c1 /= '-701') STOP 89
    if (any (x6%c2 /= ['-702','-703','-704'])) STOP 90
    if (x7(1)%c1 /= '33001') STOP 91
    if (x7(2)%c1 /= '33002') STOP 92
    if (any (x7(1)%c2 /= ['44001','44002','44003'])) STOP 93
    if (any (x7(2)%c2 /= ['44011','44012','44013'])) STOP 94

    if (x8%c1 /= '-601') STOP 95
    if (any(x8%c2 /= ['-602','6703','-604'])) STOP 96
    if (x9(1)%c1 /= '35001') STOP 97
    if (x9(2)%c1 /= '35002') STOP 98
    if (any (x9(1)%c2 /= ['45001','45002','45003'])) STOP 99
    if (any (x9(2)%c2 /= ['45011','45012','45013'])) STOP 100
 
    if (x10%c1 /= '-501') STOP 101
    if (any (x10%c2 /= ['-502','-503','-504'])) STOP 102
    if (x11(1)%c1 /= '36001') STOP 103
    if (x11(2)%c1 /= '36002') STOP 104
    if (any (x11(1)%c2 /= ['46001','46002','46003'])) STOP 105
    if (any (x11(2)%c2 /= ['46011','46012','46013'])) STOP 106

    if (any (x5 /= [ 'x5-42', 'x5-53' ])) STOP 107

    if (x12(1)%c1 /= '37001') STOP 108
    if (x12(2)%c1 /= '37002') STOP 109
    if (any (x12(1)%c2 /= ['47001','47002','47003'])) STOP 110
    if (any (x12(2)%c2 /= ['47011','47012','47013'])) STOP 111
  end subroutine test4
end program nml_test
