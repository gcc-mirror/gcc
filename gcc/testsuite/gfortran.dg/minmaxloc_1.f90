! { dg-do run }
! Check max/minloc.
! PR fortran/31726
!
program test
  implicit none
  integer :: i(1), j(-1:1), res(1)
  logical, volatile :: m(3), m2(3)
  m = (/ .false., .false., .false. /)
  m2 = (/ .false., .true., .false. /)
  call check(1, 0, MAXLOC((/ 42, 23, 11 /), DIM=1, MASK=.FALSE.))
  call check(2, 0, MAXLOC((/ 42, 23, 11 /), DIM=1, MASK=m))
  call check(3, 2, MAXLOC((/ 42, 23, 11 /), DIM=1, MASK=m2))
  call check(4, 0, MAXLOC(i(1:0), DIM=1, MASK=.TRUE.))
  call check(5, 0, MAXLOC(i(1:0), DIM=1, MASK=.FALSE.))
  call check(6, 0, MAXLOC(i(1:0), DIM=1, MASK=m(1:0)))
  call check(7, 0, MAXLOC(i(1:0), DIM=1))
  call check(8, 0, MINLOC((/ 42, 23, 11 /), DIM=1, MASK=.FALSE.))
  call check(9, 0, MINLOC((/ 42, 23, 11 /), DIM=1, MASK=m))
  call check(10, 0, MINLOC(i(1:0), DIM=1, MASK=.FALSE.))
  call check(11,0, MINLOC(i(1:0), DIM=1, MASK=m(1:0)))
  call check(12,0, MINLOC(i(1:0), DIM=1, MASK=.TRUE.))
  call check(13,0, MINLOC(i(1:0), DIM=1))

  j = (/ 1, 2, 1 /); call check(14, 2, MAXLOC(j, DIM=1))
  j = (/ 1, 2, 3 /); call check(15, 3, MAXLOC(j, DIM=1))
  j = (/ 3, 2, 1 /); call check(16, 1, MAXLOC(j, DIM=1))
  j = (/ 1, 2, 1 /); call check(17, 1, MINLOC(j, DIM=1))
  j = (/ 1, 2, 3 /); call check(18, 1, MINLOC(j, DIM=1))
  j = (/ 3, 2, 1 /); call check(19, 3, MINLOC(j, DIM=1))

  j = (/ 1, 2, 1 /); call check(20, 2, MAXLOC(j, DIM=1,mask=.true.))
  j = (/ 1, 2, 3 /); call check(21, 3, MAXLOC(j, DIM=1,mask=.true.))
  j = (/ 3, 2, 1 /); call check(22, 1, MAXLOC(j, DIM=1,mask=.true.))
  j = (/ 1, 2, 1 /); call check(23, 1, MINLOC(j, DIM=1,mask=.true.))
  j = (/ 1, 2, 3 /); call check(24, 1, MINLOC(j, DIM=1,mask=.true.))
  j = (/ 3, 2, 1 /); call check(25, 3, MINLOC(j, DIM=1,mask=.true.))

  j = (/ 1, 2, 1 /); call check(26, 0, MAXLOC(j, DIM=1,mask=.false.))
  j = (/ 1, 2, 3 /); call check(27, 0, MAXLOC(j, DIM=1,mask=.false.))
  j = (/ 3, 2, 1 /); call check(28, 0, MAXLOC(j, DIM=1,mask=.false.))
  j = (/ 1, 2, 1 /); call check(29, 0, MINLOC(j, DIM=1,mask=.false.))
  j = (/ 1, 2, 3 /); call check(30, 0, MINLOC(j, DIM=1,mask=.false.))
  j = (/ 3, 2, 1 /); call check(31, 0, MINLOC(j, DIM=1,mask=.false.))

  j = (/ 1, 2, 1 /); call check(32, 0, MAXLOC(j, DIM=1,mask=m))
  j = (/ 1, 2, 3 /); call check(33, 0, MAXLOC(j, DIM=1,mask=m))
  j = (/ 3, 2, 1 /); call check(34, 0, MAXLOC(j, DIM=1,mask=m))
  j = (/ 1, 2, 1 /); call check(35, 0, MINLOC(j, DIM=1,mask=m))
  j = (/ 1, 2, 3 /); call check(36, 0, MINLOC(j, DIM=1,mask=m))
  j = (/ 3, 2, 1 /); call check(37, 0, MINLOC(j, DIM=1,mask=m))

  j = (/ 1, 2, 1 /); call check(38, 2, MAXLOC(j, DIM=1,mask=m2))
  j = (/ 1, 2, 3 /); call check(39, 2, MAXLOC(j, DIM=1,mask=m2))
  j = (/ 3, 2, 1 /); call check(40, 2, MAXLOC(j, DIM=1,mask=m2))
  j = (/ 1, 2, 1 /); call check(41, 2, MINLOC(j, DIM=1,mask=m2))
  j = (/ 1, 2, 3 /); call check(42, 2, MINLOC(j, DIM=1,mask=m2))
  j = (/ 3, 2, 1 /); call check(43, 2, MINLOC(j, DIM=1,mask=m2))

! Check the library minloc and maxloc
  res = MAXLOC((/ 42, 23, 11 /), MASK=.FALSE.); call check(44, 0,  res(1))
  res = MAXLOC((/ 42, 23, 11 /), MASK=m); call check(45, 0,  res(1))
  res = MAXLOC((/ 42, 23, 11 /), MASK=m2); call check(46, 2,  res(1))
  res = MAXLOC(i(1:0), MASK=.TRUE.); call check(47, 0,  res(1))
  res = MAXLOC(i(1:0), MASK=.FALSE.); call check(48, 0,  res(1))
  res = MAXLOC(i(1:0), MASK=m(1:0)); call check(49, 0,  res(1))
  res = MAXLOC(i(1:0)); call check(50, 0,  res(1))
  res = MINLOC((/ 42, 23, 11 /), MASK=.FALSE.); call check(51, 0, res(1))
  res = MINLOC((/ 42, 23, 11 /), MASK=m); call check(52, 0, res(1))
  res = MINLOC(i(1:0), MASK=.FALSE.); call check(53, 0, res(1))
  res = MINLOC(i(1:0), MASK=m(1:0)); call check(54,0, res(1))
  res = MINLOC(i(1:0), MASK=.TRUE.); call check(55,0, res(1))
  res = MINLOC(i(1:0)); call check(56,0, res(1))

  j = (/ 1, 2, 1 /); res = MAXLOC(j); call check(57, 2,  res(1))
  j = (/ 1, 2, 3 /); res = MAXLOC(j); call check(58, 3,  res(1))
  j = (/ 3, 2, 1 /); res = MAXLOC(j); call check(59, 1,  res(1))
  j = (/ 1, 2, 1 /); res = MINLOC(j); call check(60, 1, res(1))
  j = (/ 1, 2, 3 /); res = MINLOC(j); call check(61, 1, res(1))
  j = (/ 3, 2, 1 /); res = MINLOC(j); call check(62, 3, res(1))

  j = (/ 1, 2, 1 /); res = MAXLOC(j,mask=.true.); call check(63, 2,  res(1))
  j = (/ 1, 2, 3 /); res = MAXLOC(j,mask=.true.); call check(65, 3,  res(1))
  j = (/ 3, 2, 1 /); res = MAXLOC(j,mask=.true.); call check(66, 1,  res(1))
  j = (/ 1, 2, 1 /); res = MINLOC(j,mask=.true.); call check(67, 1, res(1))
  j = (/ 1, 2, 3 /); res = MINLOC(j,mask=.true.); call check(68, 1, res(1))
  j = (/ 3, 2, 1 /); res = MINLOC(j,mask=.true.); call check(69, 3, res(1))

  j = (/ 1, 2, 1 /); res = MAXLOC(j,mask=.false.); call check(70, 0,  res(1))
  j = (/ 1, 2, 3 /); res = MAXLOC(j,mask=.false.); call check(71, 0,  res(1))
  j = (/ 3, 2, 1 /); res = MAXLOC(j,mask=.false.); call check(72, 0,  res(1))
  j = (/ 1, 2, 1 /); res = MINLOC(j,mask=.false.); call check(73, 0, res(1))
  j = (/ 1, 2, 3 /); res = MINLOC(j,mask=.false.); call check(74, 0, res(1))
  j = (/ 3, 2, 1 /); res = MINLOC(j,mask=.false.); call check(75, 0, res(1))

  j = (/ 1, 2, 1 /); res = MAXLOC(j,mask=m); call check(76, 0,  res(1))
  j = (/ 1, 2, 3 /); res = MAXLOC(j,mask=m); call check(77, 0,  res(1))
  j = (/ 3, 2, 1 /); res = MAXLOC(j,mask=m); call check(78, 0,  res(1))
  j = (/ 1, 2, 1 /); res = MINLOC(j,mask=m); call check(79, 0, res(1))
  j = (/ 1, 2, 3 /); res = MINLOC(j,mask=m); call check(80, 0, res(1))
  j = (/ 3, 2, 1 /); res = MINLOC(j,mask=m);call check(81, 0, res(1))

  j = (/ 1, 2, 1 /); res = MAXLOC(j,mask=m2); call check(82, 2,  res(1))
  j = (/ 1, 2, 3 /); res = MAXLOC(j,mask=m2); call check(83, 2,  res(1))
  j = (/ 3, 2, 1 /); res = MAXLOC(j,mask=m2); call check(84, 2,  res(1))
  j = (/ 1, 2, 1 /); res = MINLOC(j,mask=m2); call check(85, 2, res(1))
  j = (/ 1, 2, 3 /); res = MINLOC(j,mask=m2); call check(86, 2, res(1))
  j = (/ 3, 2, 1 /); res = MINLOC(j,mask=m2); call check(87, 2, res(1))

contains
subroutine check(n, i,j)
  integer, value, intent(in) :: i,j,n
  if(i /= j) then
     STOP 1
!    print *, 'ERROR: Test',n,' expected ',i,' received ', j
  end if
end subroutine check
end program
