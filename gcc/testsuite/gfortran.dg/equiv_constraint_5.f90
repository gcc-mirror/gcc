! { dg-do compile }
! { dg-options "-O0" }
! PR20902 - Structure with default initializer cannot be equivalence memeber.
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
TYPE T1
 sequence
 integer :: i=1
END TYPE T1
TYPE T2
 sequence
 integer :: i      ! drop original initializer to pick up error below.
END TYPE T2
TYPE(T1) :: a1
TYPE(T2) :: a2
EQUIVALENCE(a1,a2) ! { dg-error "initializer cannot be an EQUIVALENCE" }
write(6,*) a1,a2
END

