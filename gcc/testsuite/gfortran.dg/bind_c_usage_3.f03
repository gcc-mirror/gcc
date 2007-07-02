! { dg-do compile }
module test
  use, intrinsic :: iso_c_binding

  type, bind(c) :: my_c_type ! { dg-error "BIND.C. derived type" }
     integer(c_int), pointer :: ptr ! { dg-error "cannot have the POINTER attribute" }
  end type my_c_type 
  
  type, bind(c) :: my_type ! { dg-error "BIND.C. derived type" }
     integer(c_int), allocatable :: ptr(:) ! { dg-error "cannot have the ALLOCATABLE attribute" }
  end type my_type

  type foo ! { dg-error "must have the BIND attribute" }
    integer(c_int) :: p 
  end type foo 

  type(foo), bind(c) :: cp ! { dg-error "is not C interoperable" }
  real(c_double), pointer,bind(c) :: p ! { dg-error "cannot have both the POINTER and BIND.C." }
end module test
