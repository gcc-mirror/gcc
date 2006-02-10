! PR 20894
! { dg-do compile }
! Originally contributed by Joost VandeVondele
INTEGER, POINTER :: I,J
INTEGER :: K
ALLOCATE(I)
J=>(I)   ! { dg-error "Pointer assignment target is neither TARGET nor POINTER" }
END
