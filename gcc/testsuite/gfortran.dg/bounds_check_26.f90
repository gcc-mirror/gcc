! { dg-do compile }
! { dg-options "-fcheck=bounds -fdump-tree-original" }
!
! PR fortran/119118

program main
  implicit none
  character(10) :: str = "1234567890"
  integer       :: n   = 3

  print *,      str(-1:-2)  ! zero-length substring: OK

  print *,      str(-1:n)   ! 2 checked bounds
  print *, len (str(-1:n))  ! 2 checked bounds

  print *,      str(-n:1)   ! 1 checked bound / 1 eliminated
  print *, len (str(-n:1))  ! 1 checked bound / 1 eliminated

  print *,      str(-n:11)  ! 2 checked bounds
  print *, len (str(-n:11)) ! 2 checked bounds

  print *,      str(-n*n%kind:sum(n-[0,n%kind])) ! 2 checked bounds

end program main

! { dg-final { scan-tree-dump-times "Substring out of bounds:" 12 "original" } }
