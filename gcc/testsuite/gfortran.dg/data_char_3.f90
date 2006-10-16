! { dg-do run }
! { dg-options "-O2" }
! Tests the fix PR29392, in which the iterator valued substring
! reference would cause a segfault.
!
! Contributed by Francois-Xavier Coudert  <fxcoudert@gcc.gnu.org>   
!
  character(LEN=2) :: a(2) 
  data ((a(I)(k:k),I=1,2),k=1,2) /2*'a',2*'z'/ 
  IF (ANY(a.NE."az")) CALL ABORT() 
  END 
