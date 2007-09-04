! { dg-do run }
! Various runtime tests of PROCEDURE declarations.
! Contributed by Janus Weil <jaydub66@gmail.com>

module m

  abstract interface
    subroutine csub() bind(c)
    end subroutine csub
  end interface

  procedure():: mp1
  procedure(real), private:: mp2
  procedure(mfun), public:: mp3
  procedure(csub), public, bind(c) :: c, d
  procedure(csub), public, bind(c, name="myB") :: b

contains

  real function mfun(x,y)
    real x,y
    mfun=4.2
  end function

  subroutine bar(a,b)
    implicit none
    interface
      subroutine a()
      end subroutine a
    end interface
    optional ::  a
    procedure(a), optional :: b
  end subroutine bar

end module


program p
  implicit none

  abstract interface
    subroutine abssub(x)
      real x
    end subroutine
  end interface

  integer i
  real r

  procedure(integer):: p1
  procedure(fun):: p2
  procedure(abssub):: p3
  procedure(sub):: p4
  procedure():: p5
  procedure(p4):: p6
  procedure(integer) :: p7

  i=p1()
  if (i /= 5) call abort()
  i=p2(3.1)
  if (i /= 3) call abort()
  r=4.2
  call p3(r)
  if (abs(r-5.2)>1e-6) call abort()
  call p4(r)
  if (abs(r-3.7)>1e-6) call abort()
  call p5()
  call p6(r)
  if (abs(r-7.4)>1e-6) call abort()
  i=p7(4)
  if (i /= -8) call abort()
  r=dummytest(p3)
  if (abs(r-2.1)>1e-6) call abort()

contains

  integer function fun(x)
    real x
    fun=7
  end function

  subroutine sub(x)
    real x
  end subroutine

  real function dummytest(dp)
    procedure(abssub):: dp
    real y
    y=1.1
    call dp(y)
    dummytest=y
  end function

end program p


integer function p1()
  p1 = 5
end function

integer function p2(x)
  real x
  p2 = int(x)
end function

subroutine p3(x)
  real,intent(inout):: x
  x=x+1.0
end subroutine

subroutine p4(x)
  real,intent(inout):: x
  x=x-1.5
end subroutine

subroutine p5()
end subroutine

subroutine p6(x)
  real,intent(inout):: x
  x=x*2.
end subroutine

function p7(x)
 implicit none
 integer :: x, p7
 p7 = x*(-2)
end function
