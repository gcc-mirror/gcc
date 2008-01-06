! { dg-do compile }
!
! PR fortran/34655
!
! Check for F2003's 5.5.2.5 Restrictions on common and equivalence
! Test case contributed by Joost VandeVondele.
!
implicit none
type data_type
 sequence
 integer :: I = 7
end type data_type


type data_type2
 sequence
 integer :: I
end type data_type2

type(data_type) :: dd, ff
type(data_type2) :: gg
integer :: j, k, m
EQUIVALENCE(dd,J) ! { dg-error "with default initialization cannot be in EQUIVALENCE with a variable in COMMON" }
EQUIVALENCE(ff,k)
EQUIVALENCE(gg,m)
COMMON /COM/ j
COMMON /COM/ m
END
