! { dg-do run }
! PR 15140: we used to fail an assertion, because we don't use the
! argument of the subroutine directly, but instead use a copy of it.
function M(NAMES)
  CHARACTER*(*) NAMES(*)
  if (any(names.ne."asdfg")) call abort
  m = LEN(NAMES(1))
END function M

character(5) :: c(2)
c = "asdfg"
if(m(c).ne.5) call abort()
end
