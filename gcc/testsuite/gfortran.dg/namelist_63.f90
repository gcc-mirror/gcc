! { dg-do compile }
! { dg-options -std=f95 }
! PR fortran/45530, updated for PR78659
!
! Contributed by david.sagan@gmail.com
!
program test
implicit none

type c_struct
  type (g_struct), pointer :: g
end type

type g_struct
  type (p_struct), pointer :: p
end type

type p_struct
  type (region_struct), pointer :: r
end type

type region_struct
  type (p_struct) plot
end type

type (c_struct) curve(10)
! The following is allowed with f2003.
namelist / params / curve ! { dg-error "ALLOCATABLE or POINTER components" }
end program
