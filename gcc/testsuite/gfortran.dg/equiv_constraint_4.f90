! { dg-do run }
! { dg-options "-O0" }
! PR20901 - check that derived/numeric equivalence works with std!=f95.
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
TYPE data_type
 SEQUENCE
 INTEGER :: I
END TYPE data_type
INTEGER :: J = 7
TYPE(data_type) :: dd
EQUIVALENCE(dd,J)
if (dd%i.ne.7) STOP 1
END



