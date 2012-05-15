! { dg-do run }
! { dg-options "-fwhole-file -O3" }
! Check that the derived types are correctly substituted when
! whole file compiling.
!
! Contributed by Dominique d'Humieres  <dominiq@lps.ens.fr
!
module global
 type                                ::  mytype
   type(mytype),pointer   ::  this
 end type mytype
 type(mytype),target        :: base
end module global

program test_equi
  use global
  call check()
  print *, "base%this%this=>base?"  ,  associated(base%this%this,base)
  print *, "base%this%this=>?" ,          associated(base%this%this)
  print *, "base%this=>?" ,                   associated(base%this)
contains
  subroutine check()
    type(mytype),target        :: j
    base%this => j                      !have the variables point
    j%this => base                      !to one another
  end subroutine check                  !take j out of scope
end program test_equi
