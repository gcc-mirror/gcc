subroutine atom(sol,k,eval)
real, intent(in) :: sol
integer, intent(in) :: k(2)
real, intent(out) :: eval(2)
real t1
  t1=sqrt(dble(k(1)**2)-(sol)**2)
  eval(1)=sol**2/sqrt(t1)-sol**2
end subroutine

