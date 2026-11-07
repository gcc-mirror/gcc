! { dg-do run }
! { dg-options "-fcheck=all" }
! { dg-shouldfail "Parameter mismatch" }
! F2023 9.7.1.2 p10: a type-spec length parameter differing from the
! allocate-object's nondeferred parameter is an error condition, so STAT=
! must come back nonzero.  gfortran returns 0 and allocates anyway.
program p
  type :: t(n)
    integer, len :: n = 1
    integer :: v(n)
  end type
  type(t(3)), allocatable :: a
  integer :: st, parm = 5, i
  character(len=80) :: msg
  st = 0
  msg = ''
! First use a constant value for n.
  allocate (t(4) :: a, stat=st)
  if (st == 0) stop 1
  if (allocated (a)) stop 2

  allocate (t(4) :: a, stat=st, errmsg=msg)
  if (st == 0) stop 3
  if (allocated (a)) stop 4
  if (len (trim (msg)) == 0) stop 5

  st = 0
  i = 4
  msg = ''
  allocate (t(i) :: a, stat=st)
  if (st == 0) stop 6
  if (allocated (a)) stop 7

! And now use a variable value for n.
  allocate (t(i) :: a, stat=st, errmsg=msg)
  if (st == 0) stop 8
  if (allocated (a)) stop 9
  if (len (trim (msg)) == 0) stop 10
  allocate (t(i) :: a)
  if (allocated (a)) deallocate (a)
end program
