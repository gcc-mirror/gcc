! { dg-do run }

! PR fortran/37099
! Check for correct results when comparing array-section-substrings.

! This is the test from comment #1 of the PR.
! Contributed by Dominique d'Humieres <dominiq@lps.ens.fr>

integer, parameter :: n = 10
integer, parameter :: ilst(n) = (/(i,i=1,n)/)
character(*), parameter :: c0lst(n) = (/(char(96+i),i=1,n)/)
character(*), parameter :: c1lst(n) = (/(char(96+i)//'b',i=1,n)/)
logical :: tmp(n)
i = 5
print *, ilst(:) == i
print *, c0lst(:)(1:1) == char(96+i)
tmp = c1lst(:)(1:1) == char(96+i)
print *, tmp
print *, c1lst(:)(1:1) == 'e'
if (any(tmp .neqv. (c0lst(:)(1:1) == char(96+i)))) call abort()
end
