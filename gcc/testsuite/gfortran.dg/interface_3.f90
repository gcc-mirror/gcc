! { dg-do compile }
! Tests the fix for PR20880, which was due to failure to the failure
! to detect the USE association of a nameless interface for a
! procedure with the same name as the encompassing scope.
!
! Contributed by Joost VandeVondele  <jv244@cam.ac.uk>
!
module test_mod
interface
  subroutine my_sub (a)
    real a
  end subroutine
end interface
interface
  function my_fun (a)
    real a, my_fun
  end function
end interface
end module

module test_mod2
interface
  function my_fun (a)
    real a, my_fun
  end function
end interface
end module


! This is the original PR, excepting that the error requires the symbol
! to be referenced.
subroutine my_sub (a)
  use test_mod
  real a
  call my_sub (a)  ! { dg-error "ambiguous reference" }
  print *, a
end subroutine

integer function my_fun (a)
  use test_mod
  real a
  print *, a
  my_fun = 1  ! { dg-error "ambiguous reference" }
end function

! This was found whilst investigating => segfault
subroutine thy_sub (a)
  interface 
    subroutine thy_sub (a) ! { dg-error "enclosing procedure" }
      real a
    end subroutine
  end interface
  real a
  print *, a
end subroutine

subroutine thy_fun (a)
  use test_mod
  use test_mod2  ! OK because there is no reference to my_fun
  print *, a
end subroutine thy_fun

subroutine his_fun (a)
  use test_mod
  use test_mod2
  print *, my_fun (a)  ! { dg-error "ambiguous reference" }
end subroutine his_fun

! { dg-final { cleanup-modules "test_mod test_mod2" } }
