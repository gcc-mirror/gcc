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
  if (any (a /= ['aa01','aa02'])) call abort()
  if (any (ap /= ['98', '99'])) call abort()
  if (b /= '7') call abort()
  if (bp /= '101') call abort()
  if (c /= '8') call abort()
  if (any (d /= ['-1', '-2', '-3'])) call abort()

  if (e%c1 /= '-701') call abort()
  if (any (e%c2 /= ['-702','-703','-704'])) call abort()
  if (f(1)%c1 /= '33001') call abort()
  if (f(2)%c1 /= '33002') call abort()
  if (any (f(1)%c2 /= ['44001','44002','44003'])) call abort()
  if (any (f(2)%c2 /= ['44011','44012','44013'])) call abort()

  if (g%c1 /= '-601') call abort()
  if (any(g%c2 /= ['-602','6703','-604'])) call abort()
  if (h(1)%c1 /= '35001') call abort()
  if (h(2)%c1 /= '35002') call abort()
  if (any (h(1)%c2 /= ['45001','45002','45003'])) call abort()
  if (any (h(2)%c2 /= ['45011','45012','45013'])) call abort()

  if (i%c1 /= '-501') call abort()
  if (any (i%c2 /= ['-502','-503','-504'])) call abort()
  if (j(1)%c1 /= '36001') call abort()
  if (j(2)%c1 /= '36002') call abort()
  if (any (j(1)%c2 /= ['46001','46002','46003'])) call abort()
  if (any (j(2)%c2 /= ['46011','46012','46013'])) call abort()

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
    if (any (x1 /= ['aa01','aa02'])) call abort()
    if (any (x1p /= ['98', '99'])) call abort()
    if (x2 /= '7') call abort()
    if (x2p /= '101') call abort()
    if (x3 /= '8') call abort()
    if (any (x4 /= ['-1', '-2', '-3'])) call abort()

    if (x6%c1 /= '-701') call abort()
    if (any (x6%c2 /= ['-702','-703','-704'])) call abort()
    if (x7(1)%c1 /= '33001') call abort()
    if (x7(2)%c1 /= '33002') call abort()
    if (any (x7(1)%c2 /= ['44001','44002','44003'])) call abort()
    if (any (x7(2)%c2 /= ['44011','44012','44013'])) call abort()

    if (x8%c1 /= '-601') call abort()
    if (any(x8%c2 /= ['-602','6703','-604'])) call abort()
    if (x9(1)%c1 /= '35001') call abort()
    if (x9(2)%c1 /= '35002') call abort()
    if (any (x9(1)%c2 /= ['45001','45002','45003'])) call abort()
    if (any (x9(2)%c2 /= ['45011','45012','45013'])) call abort()
 
    if (x10%c1 /= '-501') call abort()
    if (any (x10%c2 /= ['-502','-503','-504'])) call abort()
    if (x11(1)%c1 /= '36001') call abort()
    if (x11(2)%c1 /= '36002') call abort()
    if (any (x11(1)%c2 /= ['46001','46002','46003'])) call abort()
    if (any (x11(2)%c2 /= ['46011','46012','46013'])) call abort()

    if (any (x5 /= [ 'x5-42', 'x5-53' ])) call abort()

    if (x12(1)%c1 /= '37001') call abort()
    if (x12(2)%c1 /= '37002') call abort()
    if (any (x12(1)%c2 /= ['47001','47002','47003'])) call abort()
    if (any (x12(2)%c2 /= ['47011','47012','47013'])) call abort()
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
    if (any (x1 /= ['aa01','aa02'])) call abort()
    if (any (x1p /= ['98', '99'])) call abort()
    if (x2 /= '7') call abort()
    if (x2p /= '101') call abort()
    if (x3 /= '8') call abort()
    if (any (x4 /= ['-1', '-2', '-3'])) call abort()

    if (x6%c1 /= '-701') call abort()
    if (any (x6%c2 /= ['-702','-703','-704'])) call abort()
    if (x7(1)%c1 /= '33001') call abort()
    if (x7(2)%c1 /= '33002') call abort()
    if (any (x7(1)%c2 /= ['44001','44002','44003'])) call abort()
    if (any (x7(2)%c2 /= ['44011','44012','44013'])) call abort()

    if (x8%c1 /= '-601') call abort()
    if (any(x8%c2 /= ['-602','6703','-604'])) call abort()
    if (x9(1)%c1 /= '35001') call abort()
    if (x9(2)%c1 /= '35002') call abort()
    if (any (x9(1)%c2 /= ['45001','45002','45003'])) call abort()
    if (any (x9(2)%c2 /= ['45011','45012','45013'])) call abort()
 
    if (x10%c1 /= '-501') call abort()
    if (any (x10%c2 /= ['-502','-503','-504'])) call abort()
    if (x11(1)%c1 /= '36001') call abort()
    if (x11(2)%c1 /= '36002') call abort()
    if (any (x11(1)%c2 /= ['46001','46002','46003'])) call abort()
    if (any (x11(2)%c2 /= ['46011','46012','46013'])) call abort()

    if (any (x5 /= [ 'x5-42', 'x5-53' ])) call abort()

    if (x12(1)%c1 /= '37001') call abort()
    if (x12(2)%c1 /= '37002') call abort()
    if (any (x12(1)%c2 /= ['47001','47002','47003'])) call abort()
    if (any (x12(2)%c2 /= ['47011','47012','47013'])) call abort()
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
    if (any (x1 /= ['aa01','aa02'])) call abort()
    if (any (x1p /= ['98', '99'])) call abort()
    if (x2 /= '7') call abort()
    if (x2p /= '101') call abort()
    if (x3 /= '8') call abort()
    if (any (x4 /= ['-1', '-2', '-3'])) call abort()

    if (x6%c1 /= '-701') call abort()
    if (any (x6%c2 /= ['-702','-703','-704'])) call abort()
    if (x7(1)%c1 /= '33001') call abort()
    if (x7(2)%c1 /= '33002') call abort()
    if (any (x7(1)%c2 /= ['44001','44002','44003'])) call abort()
    if (any (x7(2)%c2 /= ['44011','44012','44013'])) call abort()

    if (x8%c1 /= '-601') call abort()
    if (any(x8%c2 /= ['-602','6703','-604'])) call abort()
    if (x9(1)%c1 /= '35001') call abort()
    if (x9(2)%c1 /= '35002') call abort()
    if (any (x9(1)%c2 /= ['45001','45002','45003'])) call abort()
    if (any (x9(2)%c2 /= ['45011','45012','45013'])) call abort()
 
    if (x10%c1 /= '-501') call abort()
    if (any (x10%c2 /= ['-502','-503','-504'])) call abort()
    if (x11(1)%c1 /= '36001') call abort()
    if (x11(2)%c1 /= '36002') call abort()
    if (any (x11(1)%c2 /= ['46001','46002','46003'])) call abort()
    if (any (x11(2)%c2 /= ['46011','46012','46013'])) call abort()

    if (any (x5 /= [ 'x5-42', 'x5-53' ])) call abort()

    if (x12(1)%c1 /= '37001') call abort()
    if (x12(2)%c1 /= '37002') call abort()
    if (any (x12(1)%c2 /= ['47001','47002','47003'])) call abort()
    if (any (x12(2)%c2 /= ['47011','47012','47013'])) call abort()
  end subroutine test4
end program nml_test
