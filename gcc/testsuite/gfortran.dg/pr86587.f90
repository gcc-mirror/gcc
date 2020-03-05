! { dg-do compile }
! PR fortran/86587
! Code contirubted by Valentin Clement <valentin.clement at env dot ethz dot ch>
!
module mod1
   use iso_c_binding
   type, bind(c), private :: mytype
      integer(c_int) :: i1, i2
   end type
end module mod1

module mod2
  use iso_c_binding
  private
  type, bind(c) :: mytype
    integer(c_int) :: i1, i2
  end type
end module mod2
