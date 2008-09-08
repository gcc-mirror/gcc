! { dg-do run }

! PR fortran/37099
! Check for correct results when comparing array-section-substrings.

! This is the original test from the PR.
! Contributed by Dominique d'Humieres <dominiq@lps.ens.fr>

module xparams
  integer,parameter :: exprbeg=100,exprend=154
  character(*),dimension(exprbeg:exprend),parameter :: &
      exprs=(/'nint()  ','log10() ','sqrt()  ','acos()  ','asin()  ',   &
      'atan()  ','cosh()  ','sinh()  ','tanh()  ','int()   ',           &
      'cos()   ','sin()   ','tan()   ','exp()   ','log()   ','abs()   ',&
      'delta() ','step()  ','rect()  ','max(,)  ','min(,)  ','bj0()   ',&
      'bj1()   ','bjn(,)  ','by0()   ','by1()   ','byn(,)  ','logb(,) ',&
      'erf()   ','erfc()  ','lgamma()','gamma() ','csch()  ','sech()  ',&
      'coth()  ','lif(,,) ','gaus()  ','sinc()  ','atan2(,)','mod(,)  ',&
      'nthrt(,)','ramp()  ','fbi()   ','fbiq()  ','uran(,) ','aif(,,,)',&
      'sgn()   ','cbrt()  ','fact()  ','somb()  ','bk0()   ','bk1()   ',&
      'bkn(,)  ','bbi(,,) ','bbiq(,,)'/)
  logical :: tmp(55,26)
  character(26) :: al = 'abcdefghijklmnopqrstuvwxyz'
end

program pack_bug
  use xparams
    do i = 1, 1
      tmp(:,i) = (exprs(:)(1:1)==al(i:i))
      print '(55L1)', exprs(:)(1:1)=='a'
      print '(55L1)', tmp(:,i)

      if (any ((exprs(:)(1:1)=='a') .neqv. tmp(:,i))) then
        call abort ()
      end if
    end do
end

! { dg-final { cleanup-modules "xparams" } }
