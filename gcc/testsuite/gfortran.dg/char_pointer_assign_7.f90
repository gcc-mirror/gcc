! { dg-do compile }
! PR fortran/50549 - should reject pointer assignments of different lengths
! in structure constructors

program test
  implicit none
  type t
     character(2), pointer ::  p2
  end type t
  type t2
     character(2), pointer ::  p(:)
  end type t2
  type td
     character(:), pointer ::  pd
  end type td
  interface
     function f1 ()
       character(1), pointer :: f1
     end function f1
     function f2 ()
       character(2), pointer :: f2
     end function f2
  end interface

  character(1),    target  ::  p1
  character(1),    pointer ::  q1(:)
  character(2),    pointer ::  q2(:)
  type(t)  :: u
  type(t2) :: u2
  type(td) :: v
  u  = t(p1)    ! { dg-error "Unequal character lengths" }
  u  = t(f1())  ! { dg-error "Unequal character lengths" }
  u  = t(f2())  ! OK
  u2 = t2(q1)   ! { dg-error "Unequal character lengths" }
  u2 = t2(q2)   ! OK
  v  = td(p1)   ! OK
  v  = td(f1()) ! OK
end
