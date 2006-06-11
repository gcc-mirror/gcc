! { dg-do compile }
! Tests patch for PR23091, in which autmatic objects caused
! an ICE if they were given the SAVE attribute.
!
! Contributed by Valera Veryazov  <valera.veryazov@teokem.lu.se>
!
Subroutine My(n1)
  integer :: myArray(n1)
  character(n1) :: ch
  save      ! OK because only allowed objects are saved globally.
  call xxx(myArray, ch)
  return
  end

Subroutine Thy(n1)
  integer, save :: myArray(n1) ! { dg-error "SAVE attribute" }
  character(n1), save :: ch ! { dg-error "SAVE attribute" }
  call xxx(myArray, ch)
  return
  end

