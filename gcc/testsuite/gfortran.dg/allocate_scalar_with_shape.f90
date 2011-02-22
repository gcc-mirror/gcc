! { dg-do compile }
! PR fortran/41940

integer, allocatable :: a
TYPE :: x
  integer, allocatable :: a
END TYPE
TYPE (x) :: y

allocate(a(4))     ! { dg-error "Shape specification for allocatable scalar" }
allocate(y%a(4))   ! { dg-error "Shape specification for allocatable scalar" }
end

