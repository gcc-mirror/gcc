! { dg-do run }
! PR fortran/86277
!
! Check proper detection of presence of optional array dummy arguments
! for zero-sized actual array arguments or array constructors:
! tests for CHARACTER

program test
  implicit none
  character(0), parameter   :: c0(0) = ""
  character(0), parameter   :: c1(1) = ""
  character(1), parameter   :: d0(0) = ""
  character(1), parameter   :: d1(1) = ""
  character(0)              :: w0(0)
  character(0)              :: w1(1)
  character(:), allocatable :: cc(:)
  integer                   :: k = 0, l = 0     ! Test/failure counter
  !
  allocate (character(0) :: cc(0))
  call a0 ()
  call a1 ()
  call a2 ()
  call a3 ()
  print *, "Total tests:", k, " failed:", l
contains
  subroutine a0 ()
    print *, "Variables as actual argument"
    call i  (c0)
    call i  (c1)
    call i  (d0)
    call i  (d1)
    call i  (w0)
    call i  (w1)
    call i  (cc)
    print *, "Array section as actual argument"
    call i  (c1(1:0))
    call i  (c1(1:0)(1:0))
    call i  (w1(1:0))
    call i  (w1(1:0)(1:0))
    call i  (cc(1:0))
    call i  (cc(1:0)(1:0))
  end subroutine a0
  !
  subroutine a1 ()
    print *, "Explicit temporary as actual argument"
    call i ((c0))
    call i ((c1))
    call i ((d0))
    call i ((d1))
    call i ((w0))
    call i ((w1))
    call i ((cc))
    call i ((c1(1:0)))
    call i ((c1(1:0)(1:0)))
    call i ((w1(1:0)))
    call i ((w1(1:0)(1:0)))
    call i ((cc(1:0)))
    call i ((cc(1:0)(1:0)))
  end subroutine a1
  !
  subroutine a2 ()
    print *, "Array constructor as actual argument"
    call i ([c0])
    call i ([c1])
    call i ([d0])
    call i ([d1])
    call i ([w0])
    call i ([w1])
    call i ([cc])
    call i ([c0,c0])
    call i ([c1,c1])
    call i ([d0,d0])
    call i ([cc,cc])
    call i ([c1(1:0)])
    call i ([c1(1:0)(1:0)])
    call i ([w1(1:0)])
    call i ([w1(1:0)(1:0)])
    call i ([cc(1:0)])
    call i ([cc(1:0)(1:0)])
  end subroutine a2
  !
  subroutine a3 ()
    print *, "Array constructor with type-spec as actual argument"
    call i ([character(0) ::   ])
    call i ([character(0) :: ""])
    call i ([character(0) :: c0])
    call i ([character(0) :: c1])
    call i ([character(0) :: d0])
    call i ([character(0) :: d1])
    call i ([character(0) :: w0])
    call i ([character(0) :: w1])
    call i ([character(0) :: cc])
    call i ([character(0) :: c0,c0])
    call i ([character(0) :: c1,c1])
    call i ([character(0) :: d0,d0])
    call i ([character(0) :: cc,cc])
    call i ([character(0) :: c1(1:0)])
    call i ([character(0) :: c1(1:0)(1:0)])
    call i ([character(0) :: w1(1:0)])
    call i ([character(0) :: w1(1:0)(1:0)])
    call i ([character(0) :: cc(1:0)])
    call i ([character(0) :: cc(1:0)(1:0)])
  end subroutine a3
  !
  subroutine i(arg)
    character(*), optional, intent(in) :: arg(:)
    logical :: t
    t = present (arg)
    k = k + 1
    print *, 'test', k, merge ("  ok", "FAIL", t)
    if (.not. t) l = l + 1
    if (.not. t) stop k
  end subroutine i
end program
