! { dg-do run }
!
! Test the F2018 generic statement
!
function cg (arg1, arg2)
  complex :: cg
  complex, intent(in) :: arg1, arg2
  cg = arg1 + arg2
end

module m
  implicit none

  type :: t
    integer :: i
  end type
  integer :: tsum = 0

  public g
  interface g  ! Check generic statement + generic interface works
    module procedure tg
  end interface g

  generic :: g => ig, rg
  generic :: operator(.plus.) => ig, rg
  generic, private :: h => ig, rg
  generic :: WRITE(FORMATTED) => wtarray

  interface g  ! Check generic statement + generic interface works
    function cg (arg1, arg2)
      complex :: cg
      complex, intent(in) :: arg1, arg2
    end
  end interface g

! Subroutines
  generic, public :: sg => sig, srg

! Check that we can mix with submodule procedures
  interface
    real module function realg (arg1, arg2)
      real, intent(in) :: arg1, arg2
    end function
  end interface
  generic, public :: subg => ig, realg

contains

  function rg (arg1, arg2)
    real :: rg
    real, intent(in) :: arg1, arg2
    rg = arg1 + arg2
  end
  function ig (arg1, arg2)
    integer :: ig
    integer, intent(in) :: arg1, arg2
    ig = arg1 + arg2
  end
  function tg (arg1, arg2) result(res)
    type(t) :: res
    type(t), intent(in) :: arg1, arg2
    res%i = arg1%i + arg2%i
  end
  subroutine srg (arg1, arg2, arg3)
    real :: arg3
    real, intent(in) :: arg1, arg2
    arg3 = arg1 + arg2
  end
  subroutine sig (arg1, arg2, arg3)
    integer :: arg3
    integer, intent(in) :: arg1, arg2
    arg3 = arg1 + arg2
  end

  SUBROUTINE wtarray (dtv, unit, iotype, v_list, iostat, iomsg)
  CLASS(t), INTENT(IN)        :: dtv 
  INTEGER, INTENT(IN)         :: unit
  CHARACTER(*), INTENT(IN)    :: iotype
  INTEGER, INTENT(IN)         :: v_list (:)
  INTEGER, INTENT(OUT)        :: iostat
  CHARACTER(*), INTENT(INOUT) :: iomsg
    WRITE (unit, FMT=*, iostat=iostat, iomsg=iomsg) dtv%i
  END SUBROUTINE wtarray

  subroutine foo
    real :: a = 1.0, b = 2.0, r
    integer :: c = 3, d = 4
    type(t) :: tres
    generic :: operator(+) => tg
!   private in foo
    r = h(a,b)
    if (r /= rg(a,b)) stop 1
    if (h(c,d) /= ig(c,d)) stop 2
!   operator in foo
    r = a.plus.b
    if (r /= rg(a,b)) stop 3
    if ((c.plus.(2*d)) /= ig(c,2*d)) stop 4
!   check intrinsic operator
    tres = t(21) + t(21)
    if (tres%i /= 42) stop 5
  end
end module m

submodule (m) subm
contains
  real module function realg (arg1, arg2)
    real, intent(in) :: arg1, arg2
    realg = arg1 + arg2
  end
end

program p
  use m
  implicit none
  integer :: i, rv

  generic :: operator(.minus.) => pig, prg
  generic :: operator(*) => times
  generic :: j => ig, rg
  generic :: j => mg

  real :: a = 1.0, b = 2.0, s3
  integer :: c = 3, d = 4, si
  type(t) :: t1 = t(2), t2 = t(3), tres
  type(t) :: tarray(5) = [t(5), t(4), t(3), t(2), t(1)]

! module generic in p
  if (g(2.0*a,2.0*b) /= rg(2.0*a,2.0*b)) stop 6
  if (g(c,d) /= ig(c,d)) stop 7
! local generic in p
  if (j(a,b) /= rg(a,b)) stop 8
  if (j(c,d) /= ig (c,d)) stop 9
! local generic in p with different number of arguments
  if (j(c,d,-1) /= mg(c,d,-1)) stop 10
! module operator in p
  if (7*int(a.plus.b) /=  3*(c.plus.d)) stop 11
! local operator in p
  if ((a.minus.b) /= prg(a,b)) stop 12
  if ((c.minus.d) /= pig(c,d)) stop 13
! local operator in block
  block
    generic :: operator(.bminus.) => pig, prg
    if ((a.bminus.b) /= prg(a,b)) stop 14
    if ((c.bminus.d) /= pig(c,d)) stop 15
  end block
! intrinsic operator in p
  tres = t1 * t2
  if (tres%i /= 6) stop 16
! test private interface in module
  call foo
! test mixture of GENERIC statement and generic INTERFACE
  if (g((1.0,1.0),(2.0,2.0)) /= cg((1.0,1.0),(2.0,2.0))) stop 17
  tres = g(t1,t2)
  if (tres%i /= 5) stop 18
! subroutines
  call sg(10.0*a, b, s3)
  if (int(s3) /= 12) stop 19
  call sg(5*c, d, si)
  if (si /= 19) stop 20
! submodule procedures
  if (subg(20.0*a,2.0*b) /= realg(20.0*a,2.0*b)) stop 21
! check DTIO
  open (10,status='scratch')
  WRITE(10, '(DT)') tarray
  rewind(10)
  do i = 1,5
    read(10, *) rv
    tsum = tsum + rv
  end do
  close(10)
  if (tsum /= 15) stop 22
contains

  function pig (arg1, arg2)
    integer :: pig
    integer, intent(in) :: arg1, arg2
    pig = arg1 - arg2
  end
  function prg (arg1, arg2)
    real :: prg
    real, intent(in) :: arg1, arg2
    prg = arg1 - arg2
  end
  function times (arg1, arg2) result(res)
    type(t) :: res
    type(t), intent(in) :: arg1, arg2
    res%i = arg1%i * arg2%i
  end
  function mg (arg1, arg2, arg3)
    integer :: mg
    integer, intent(in) :: arg1, arg2, arg3
    mg = arg1 - arg2 * arg3
  end
end
