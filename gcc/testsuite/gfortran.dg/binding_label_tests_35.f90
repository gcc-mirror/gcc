! { dg-do compile }
! { dg-options "-Wsurprising" }
! PR fortran/49111
!
! Do not warn for interface declarations with C binding declared PRIVATE

module mod1
  use iso_c_binding
  implicit none
  save

  interface
     function strerror(errnum) bind(C, NAME = 'strerror')
       import
       type(C_PTR) :: strerror
       integer(C_INT), value :: errnum
     end function strerror
  end interface

  private strerror
end module mod1
