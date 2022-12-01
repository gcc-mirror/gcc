! { dg-do run }
! PR fortran/107874 - merge not using all its arguments
! Contributed by John Harper

program testmerge9
  implicit none
  integer :: i
  logical :: x(2) = (/.true., .false./)
  logical :: called(2)

  ! At run-time all arguments shall be evaluated
  do i = 1,2
     called = .false.
     print *, merge (tstuff(), fstuff(), x(i))
     if (any (.not. called)) stop 1
  end do

  ! Compile-time simplification shall not drop non-constant args
  called = .false.
  print *, merge (tstuff(),fstuff(),.true.)
  if (any (.not. called)) stop 2
  called = .false.
  print *, merge (tstuff(),fstuff(),.false.)
  if (any (.not. called)) stop 3
  called = .false.
  print *, merge (tstuff(),.false.,.true.)
  if (any (called .neqv. [.true.,.false.])) stop 4
  called = .false.
  print *, merge (tstuff(),.false.,.false.)
  if (any (called .neqv. [.true.,.false.])) stop 5
  called = .false.
  print *, merge (.true.,fstuff(),.true.)
  if (any (called .neqv. [.false.,.true.])) stop 6
  called = .false.
  print *, merge (.true.,fstuff(),.false.)
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
