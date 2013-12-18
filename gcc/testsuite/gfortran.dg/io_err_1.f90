! { dg-do run }
! { dg-shouldfail "Compile-time specifier checking" }
!
! Contributed by Dominique Dhumieres <dominiq at lps dot ens dot fr>
program read
   character(50) :: buf='0.D99999'
   double precision val
   read (UNIT=buf, FMT='(D60.0)', ERR=10) Val
   call abort
10 read (UNIT=buf, FMT='(D60.0)') Val
end program read
! { dg-output "At line 10 of file.*" }
! { dg-output "Fortran runtime error: Bad value during floating point read" }

