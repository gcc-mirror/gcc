! { dg-do run }
! PR fortran/107874 - merge not using all its arguments
! Contributed by John Harper

program testmerge9
  implicit none
  integer :: i
  logical :: x(2) = (/.true., .false./)
  logical :: called(2)
  logical :: y

  ! At run-time all arguments shall be evaluated
  do i = 1,2
     called = .false.
     y = merge (tstuff(), fstuff(), x(i))
     print *, y
     if (any (.not. called)) stop 1
  end do

  ! Compile-time simplification shall not drop non-constant args
  called = .false.
  y = merge (tstuff(),fstuff(),.true.)
  print *, y
  if (any (.not. called)) stop 2
  called = .false.
  y = merge (tstuff(),fstuff(),.false.)
  print *, y
  if (any (.not. called)) stop 3
  called = .false.
  y = merge (tstuff(),.false.,.true.)
  print *, y
  if (any (called .neqv. [.true.,.false.])) stop 4
  called = .false.
  y = merge (tstuff(),.false.,.false.)
  print *, y
  if (any (called .neqv. [.true.,.false.])) stop 5
  called = .false.
  y = merge (.true.,fstuff(),.true.)
  print *, y
  if (any (called .neqv. [.false.,.true.])) stop 6
  called = .false.
  y = merge (.true.,fstuff(),.false.)
  print *, y
  if (any (called .neqv. [.false.,.true.])) stop 7
contains
  logical function tstuff()
    print *,'tstuff'
    tstuff = .true.
    called(1) = .true.
  end function tstuff
  
  logical function fstuff()
    print *,'fstuff'
    fstuff = .false.
    called(2) = .true.
  end function fstuff
end program testmerge9
