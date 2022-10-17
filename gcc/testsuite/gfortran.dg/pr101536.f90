! { dg-do compile }
! PR fortran/101536 - ICE in gfc_conv_expr_descriptor

program p
  type s
     class(*), allocatable :: c
  end type
  type t
     class(*), allocatable :: c(:)
  end type t
  type u
     integer :: c(2)
  end type
  type(t) :: x
  x%c = [1,2,3,4]
!  print *, size (x)
  print *, size (x%c)
  print *, size (x%c(1)) ! { dg-error "must be an array" }
contains
  integer function f(x, y, z)
    class(t), allocatable :: x(:)
    class(u)              :: y(:)
    class(s)              :: z
    f = size (x)
    f = size (x(1))      ! { dg-error "must be an array" }
    f = size (y)
    f = size (y%c(1))
    f = size (y(2)%c)
    f = size (y(2)%c(1)) ! { dg-error "must be an array" }
    f = size (z)         ! { dg-error "must be an array" }
    f = size (z% c)      ! { dg-error "must be an array" }
  end
end
