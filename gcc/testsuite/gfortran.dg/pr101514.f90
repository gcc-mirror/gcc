! { dg-do compile }
! PR fortran/101514 - ICE: out of memory allocating ... bytes

subroutine s
  type t1
     integer :: a(..) ! { dg-error "must have an explicit shape" }
  end type
  type t2
     integer :: a(*)  ! { dg-error "must have an explicit shape" }
  end type
  type t3
     integer :: a(:)  ! { dg-error "must have an explicit shape" }
  end type
  type t4
     integer :: a(0:) ! { dg-error "must have an explicit shape" }
  end type
  type t5
     integer, allocatable :: a(:)
  end type
  type t6
     integer, pointer     :: a(:)
  end type
  type(t1) :: a1
  type(t2) :: a2
  type(t3) :: a3
  type(t4) :: a4
  type(t5) :: a5
  type(t6) :: a6
  a1 = transfer(1, a1)
  a2 = transfer(1, a2)
  a3 = transfer(1, a3)
  a4 = transfer(1, a4)
  a5 = transfer(1, a5)
  a6 = transfer(1, a6)
end
