! { dg-do compile }
! PR fortran/83113
module mm
  implicit none
  interface
     module function c()
       integer, dimension(2)  :: c
     end function c
  end interface
end module mm

submodule (mm) oo
  implicit none
contains
  module function c()
    integer, dimension(3)  :: c
  end function c
end submodule oo
