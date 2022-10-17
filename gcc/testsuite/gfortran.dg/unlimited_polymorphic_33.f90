! { dg-do compile }
! PR fortran/101349 - ICE in gfc_get_descriptor_field
! Check constraint F2008:C628 / F2018:C932

subroutine s(x)
  class(*) :: x(:)
  allocate (x, source=['abc']) ! { dg-error "must be ALLOCATABLE or a POINTER" }
end

subroutine t(x)
  class(*), allocatable :: x(:)
  allocate (x, source=['abc'])
end

subroutine u(x)
  class(*), pointer :: x(:)
  allocate (x, source=['abc'])
end
