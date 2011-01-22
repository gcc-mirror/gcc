! { dg-do compile }
! { dg-options "-fcoarray=single" }
! PR 38536 - array sections as arguments to c_loc are illegal.
  use iso_c_binding
  type, bind(c) :: t1
     integer(c_int) :: i(5)
  end type t1
  type, bind(c):: t2
     type(t1) :: t(5)
  end type t2
  type, bind(c) :: t3
     type(t1) :: t(5,5)
  end type t3

  type(t2), target :: tt
  type(t3), target :: ttt
  integer(c_int), target :: n(3)
  integer(c_int), target :: x[*]
  type(C_PTR) :: p

  p = c_loc(tt%t%i(1))  ! { dg-error "Array section not permitted" }
  p = c_loc(n(1:2))  ! { dg-warning "Array section" }
  p = c_loc(ttt%t(5,1:2)%i(1)) ! { dg-error "Array section not permitted" }
  p = c_loc(x[1]) ! { dg-error "Coindexed argument not permitted" }
  end
