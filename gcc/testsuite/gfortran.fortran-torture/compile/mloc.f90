! from PR 14928
! we used to not accept the two argument variant of MINLOC and MAXLOC when
! the MASK keyword was omitted.
  real b(10)
  integer c(1)
  c = minloc(b,b<0)
  c = maxloc(b,b>0)
end
