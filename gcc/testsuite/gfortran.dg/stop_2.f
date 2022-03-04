! { dg-do compile }
! { dg-options "-std=f2018" }

      implicit none
      logical      :: q = .false.
      integer(2)   :: p = 99
      real         :: x = 0.
      character(5) :: s = "stopp"
      stop 1, quiet=.false.
      stop 2, quiet=q
      stop 3, quiet=f(x)
      stop42,quiet=.false.
      error stop 4, quiet=.true.
      error stop 5 , quiet=.true.
      stop1_1;stop2_2;stop4_4;stop8_8
      stopp;stops
      st
     &op42
      stop, quiet=any([.false.])
      stop , quiet=any([f(x)])
      stop"stopp",quiet=any([f(x)])
      stop "stopp" , quiet=any([f(x)])
      s to ps,quiet=all([f(x)])
      e r r o r s t o p 4 3 , q u i e t = . t r u e .
      errorstop"stopp",quiet=.not.f(x)
      contains
      logical function f(x)
      real, intent(in) :: x
      f = .false.
      end function f
      end
