! { dg-do compile }
! PR fortran/51993
! Code contributed by Sebastien Bardeau <bardeau at iram dot fr>
module mymod
  type :: mytyp
    character(len=3) :: a = .true. ! { dg-error "convert LOGICAL" }
  end type mytyp
end module mymod
