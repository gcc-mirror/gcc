! { dg-do compile }
!
! PR fortran/37076
!
! Before the result of concatenations was always a kind=1 string
!
program test3
  integer,parameter :: u = 4
  character(1,u),parameter :: nen=char(int(z'5e74'),u) !year
  character(25,u) :: string
  string = u_"2008"//nen
  print *, u_"2008"//nen ! Compiles OK
  print *, u_"2008"//nen//u_"8" ! Rejects this.
end program test3
