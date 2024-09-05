! { dg-do compile }
! PR fortran/100273 - ICE in gfc_create_module_variable
!
! Contributed by G.Steinmetz

module m
  implicit none
contains
  character(4) function g(k)
    integer :: k
    g = f(k)
  contains
    function f(n)
      character(3), parameter :: a(2) = ['1  ', '123']
      integer :: n
      character(len_trim(a(n))) :: f
      f = 'abc'
    end
  end
end
program p
  use m 
  implicit none
  print *, '>>' // g(1) // '<<'
  print *, '>>' // g(2) // '<<'
end
