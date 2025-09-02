! { dg-do compile }
!
! Test the F2018 generic statement error reporting using the module from
! generic_stmt_1.f90
!
function cg (arg1, arg2)
  complex :: cg
  complex, intent(in) :: arg1, arg2
  cg = arg1 + arg2
end

module m1
  implicit none

  type :: t
    integer :: i
  end type

  public g
  interface g  ! Check generic statement + generic interface works
    module procedure tg
  end interface g

  generic, public :: g => ig             ! { dg-error "repeats that already given" }
  generic, private :: g => rg            ! { dg-error "conflicts with that already" }
  generic :: operator(.plus.) => ig, rg, gg ! { dg-error "did you mean|must be a FUNCTION" }
  generic, private :: h => ig, rg
  generic :: => ig, rg                      ! { dg-error "Malformed GENERIC statement" }
  generic :: wron ng => ig, rg              ! { dg-error "Expected .=>." }
  generic :: #!& => ig, rg                  ! { dg-error "Malformed GENERIC statement" }
  generic, private :: operator(.plusplus.) => ig
  generic, private :: operator(.plusplus.) => rg ! { dg-error "repeats the access specification" }
  generic, PUBLIC :: operator(.plusplus.) => tg ! { dg-error "must have the same access" }

  interface g  ! Check generic statement + generic interface works
    function cg (arg1, arg2)
      complex :: cg
      complex, intent(in) :: arg1, arg2
    end
  end interface g

  generic, public :: sg => sig, srg
  generic, public :: sg2 => sig, srg, rg     ! Error appears at 'srg' declaration


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
  subroutine srg (arg1, arg2, arg3)         ! { dg-error "procedures must be either all SUBROUTINEs" }
    real :: arg3
    real, intent(in) :: arg1, arg2
    arg3 = arg1 + arg2
  end
  subroutine sig (arg1, arg2, arg3)
    integer :: arg3
    integer, intent(in) :: arg1, arg2
    arg3 = arg1 + arg2
  end
  subroutine foo
    real :: a = 1.0, b = 2.0, r
    integer :: c = 3, d = 4
    generic, public :: sg => sig, srg       ! { dg-error "not in a module" }
    generic :: operator(+) => rg            ! { dg-error "conflicts with intrinsic interface" }
    r = h(a,d)                              ! { dg-error "There is no specific function" }
    if (r /= rg(a,b)) stop 1
    if (h(c,d) /= ig(c,d)) stop 2
    generic :: wrong => ig, rg              ! { dg-error "Unexpected GENERIC statement" }
!   operator in foo
    r = c.plus.b                            ! { dg-error "Unknown operator" }
    if (r /= rg(a,b)) stop 3
    if ((c.plus.(2*d)) /= ig(c,2*d)) stop 4
  end
end module m1
