! { dg-do run }
! Verify that the sizeof intrinsic does as advertised
subroutine check_int (j)
  INTEGER(4) :: i, ia(5), ib(5,4), ip, ipa(:)
  target :: ib
  POINTER :: ip, ipa
  logical :: l(6)
  integer(8) :: jb(5,4)

  if (sizeof (jb) /= 2*sizeof (ib)) STOP 1

  if (sizeof(j) == 4) then
     if (sizeof (j) /= sizeof (i)) STOP 2
  else
     if (sizeof (j) /= 2 * sizeof (i)) STOP 3
  end if

  ipa=>ib(2:3,1)

  l = (/ sizeof(i) == 4, sizeof(ia) == 20, sizeof(ib) == 80, &
       sizeof(ip) == 4, sizeof(ipa) == 8, sizeof(ib(1:5:2,3)) == 12 /)

  if (any(.not.l)) STOP 4

  if (sizeof(l) /= 6*sizeof(l(1))) STOP 5
end subroutine check_int

subroutine check_real (x, y)
  dimension y(5)
  real(4) :: r(20,20,20), rp(:,:)
  target :: r
  pointer :: rp
  double precision :: d(5,5)
  complex(kind=4) :: c(5)

  if (sizeof (y) /= 5*sizeof (x)) STOP 6

  if (sizeof (r) /= 8000*4) STOP 7
  rp => r(5,2:10,1:5)
  if (sizeof (rp) /= 45*4) STOP 8
  rp => r(1:5,1:5,1)
  if (sizeof (d) /= 2*sizeof (rp)) STOP 9
  if (sizeof (c(1)) /= 2*sizeof(r(1,1,1))) STOP 10
end subroutine check_real

subroutine check_derived ()
  type dt
     integer i
  end type dt
  type (dt) :: a
  integer :: i
  type foo
     integer :: i(5000)
     real :: j(5)
     type(dt) :: d
  end type foo
  type bar
     integer :: j(5000)
     real :: k(5)
     type(dt) :: d
  end type bar
  type (foo) :: oof
  type (bar) :: rab
  integer(8) :: size_500, size_200, sizev500, sizev200
  type all
     real, allocatable :: r(:)
  end type all
  real :: r(200), s(500)
  type(all) :: v

  if (sizeof(a) /= sizeof(i)) STOP 11
  if (sizeof(oof) /= sizeof(rab)) STOP 12
  allocate (v%r(500))
  sizev500 = sizeof (v)
  size_500 = sizeof (v%r)
  deallocate (v%r)
  allocate (v%r(200))
  sizev200 = sizeof (v)
  size_200 = sizeof (v%r)
  deallocate (v%r)
  if (size_500 - size_200 /= sizeof(s) - sizeof(r) .or. sizev500 /= sizev200) &
       STOP 13
end subroutine check_derived

call check_int (1)
call check_real (1.0, (/1.0, 2.0, 3.0, 4.0, 5.0/))
call check_derived ()
end
