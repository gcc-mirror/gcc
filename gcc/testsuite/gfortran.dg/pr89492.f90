! { dg-do compile }
!
! PR fortran/89492 - Endless compilation of an invalid TRANSFER after r269177
! Test error recovery for invalid uses of TRANSFER
! Test proper simplification for MOLD with size 0
!
! Derived from original testcase by Dominique d'Humieres

program bug4a
  implicit none
  type bug4
! Intentionally left empty
  end type bug4
  integer, parameter :: k = size(transfer('',['']))  ! k = 0
  integer, parameter :: i = len (transfer('',['']))  ! i = 0
  integer, parameter :: l = len (transfer('', '' ))  ! l = 0
  integer, parameter :: m(k) = k
  integer, parameter :: j(i) = i
  integer, parameter :: n(l) = l
  print *, k,i,l,m,j,n
  print *,      transfer(1,[''])                ! { dg-error "shall not have storage size 0" }
  print *,      transfer(1, '' )                ! No error
  print *, size(transfer(1,['']))               ! { dg-error "shall not have storage size 0" }
  print *, len (transfer(1, '' ))               ! No error
  print *, size(transfer([1],[bug4()]))         ! { dg-error "shall not have storage size 0" }
  print *, transfer(transfer([1],[bug4()]),[1]) ! { dg-error "shall not have storage size 0" }
end program bug4a
