! { dg-do compile }
! PR 79312 - assigning a logical value to a real
! is invalid.
! Test case by John Harper.
program emptyarray5
  implicit none
  real a(0)
  a = [logical::] ! { dg-error "Cannot convert LOGICAL" }
  print *,size(a)
end program emptyarray5
