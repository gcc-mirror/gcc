! { dg-do run }

! PR fortran/37588
! This test used to not resolve the GENERIC binding.

! Contributed by Salvatore Filippone <sfilippone@uniroma2.it>

module bar_mod

  type foo
    integer :: i
    
  contains
    procedure, pass(a) :: foo_v => foo_v_inner    
    procedure, pass(a) :: foo_m => foo_m_inner    
    generic, public    :: foo => foo_v, foo_m
  end type foo
  
  private foo_v_inner, foo_m_inner

contains
  
  subroutine foo_v_inner(x,a)
    real :: x(:)
    class(foo) :: a
    
    a%i = int(x(1))
    WRITE (*,*) "Vector"
  end subroutine foo_v_inner
  
  subroutine foo_m_inner(x,a)
    real :: x(:,:)
    class(foo) :: a
    
    a%i = int(x(1,1))
    WRITE (*,*) "Matrix"
  end subroutine foo_m_inner
end module bar_mod

program foobar
  use bar_mod
  type(foo) :: dat
  real :: x1(10), x2(10,10)

  x1=1
  x2=2
 
  call dat%foo(x1)
  call dat%foo(x2)

end program foobar

! { dg-output "Vector.*Matrix" }
