! { dg-do compile }
! PR fortran/102685

program p
  type t
     integer :: a(2)
  end type
  type(t), parameter :: x0    = t([2])         ! { dg-error "shape of component" }
  type(t), parameter :: x1(2) = t([2])         ! { dg-error "shape of component" }
  type(t), parameter :: x(2)  = t([integer::]) ! { dg-error "shape of component" }

  type u
     integer :: a
     integer :: b(0)
  end type
  type(u), parameter :: z0(2) = u(1, [integer::]) ! valid
  type(u), parameter :: z1    = u(1,  2 )         ! valid
  type(u), parameter :: z2(2) = u(1,  2 )         ! valid
  type(u), parameter :: z3    = u(1, [2])         ! { dg-error "shape of component" }
  type(u), parameter :: z4(2) = u(1, [2])         ! { dg-error "shape of component" }

  type v
     integer :: a(2,1)
  end type
  type(v), parameter :: y0   = v(reshape([1,2],[2,1])) ! valid
  type(v), parameter :: y1   = v(reshape([1,2],[1,2])) ! { dg-error "shape of component" }
  type(v), parameter :: y(1) = v(reshape([1,2],[1,2])) ! { dg-error "shape of component" }

  print *, x0,x,x1,y0,y1,y,z0,z1,z2,z3,z4
end
