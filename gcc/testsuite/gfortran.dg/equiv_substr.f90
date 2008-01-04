! { dg-do compile }
!
! PR fortran/34557
!
! Substrings with space before '(' were not properly parsed.
!
implicit none
character :: A(2,2)*2, B(2)*3, C*5
equivalence (A (2,1) (1:1), B (1) (2:3), C (3:5))
end
