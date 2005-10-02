! { dg-do compile }
! { dg-options "-std=f95" }
! PR20901 - F95 constrains mixing of types in equivalence.
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
 character(len=4) :: a
 integer :: i
 equivalence(a,i) ! { dg-error "in default CHARACTER EQUIVALENCE statement at" }
 END


