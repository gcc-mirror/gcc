! { dg-do compile }
! { dg-options "-fcoarray=lib" }
!
! PR fortran/104330 - ICE in gfc_simplify_image_index
! Contributed by G.Steinmetz

program p
  implicit none
  type t
  end type t
  class(*), allocatable :: x[:]
  class(t), allocatable :: y[:]
  type(t),  allocatable :: z[:]
  allocate (real :: x[*])
  print *, image_index(x, [1])
  allocate (t :: y[*])
  print *, image_index(y, [1])
  allocate (t :: z[*])
  print *, image_index(z, [1])
end
