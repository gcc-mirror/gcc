! { dg-do run }
! See attachment to PR82173 comment 10 - bug13-proc-ptr-comp-init-ice.f90
module ml
  type :: t(n)
    integer, len :: n = 1
    real :: v(n)
    procedure(reducer), pointer, nopass :: op => null () ! ICE
  end type
  abstract interface
    real function reducer (x)
      real, intent(in) :: x(:)
    end function
  end interface
end module
program l1
  use ml
  type(t(3)) :: a
  a%v = 1.0
  if (associated (a%op)) stop 1
  a%op => tgt
  if (.not.associated (a%op)) stop 2 ! Check normal functioning not affected
  if (a%op ([1.,2.,3.,4.]) /= 10) stop 3
  a%op => NULL ()
  if (associated (a%op)) stop 4
contains
    real function tgt (x)
      real, intent(in) :: x(:)
      tgt = sum (x)
    end function
end program
