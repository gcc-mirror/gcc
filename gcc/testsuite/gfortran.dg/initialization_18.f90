! { dg-do compile }
! { dg-options "-std=f95 -Wall" }
!
! PR fortran/34915
! Testcase contributed by Al Greynolds via comp.lang.fortran.
!

 character(*),dimension(3),parameter :: a=(/'a()  ','b(,) ','c(,,)'/)
 integer,dimension(3),parameter :: l=len_trim(a)
end
