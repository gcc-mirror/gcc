! { dg-do compile }
! { dg-options "-std=f2018" }

  implicit none
  logical      :: q = .false.
  integer(2)   :: p = 99
  real         :: x = 0.
  character(5) :: s = "stopp"
  print *, "Hello"
  stop 1, quiet=.false.
  stop 2, quiet=q
  stop 3, quiet=f(x)
  stop; stop!
  stop ;stop 4!
  stop 5; stop 6
  stop 7 ;stop 8
  stop 1_1; stop 2_2; stop 4_4; stop 8_8
  stop&!
       &;stop;&!
       stop&!
       s&
       ; stop "x";&!
       ; st&!
       &op&!
       p
  stop s
  if(f(x))then;stop 9,quiet=.false.;else;stop 10;endif
  error stop 4, quiet=.true.
  error stop 5 , quiet=.true.
  error stop s, quiet=.true.
  stop "last " // s, quiet=.false._2
  stop, quiet=any([.false.])
  stop , quiet=any([f(x)])
  stop "stopp" , quiet=any([f(x)])
  stop s, quiet=all([f(x)])
  stop42, quiet=.false.            ! { dg-error "Blank required" }
  stop"stopp" , quiet=any([f(x)])  ! { dg-error "Blank required" }
  stop 8, quiet=([f(x)])           ! { dg-error "must be a scalar LOGICAL" }
contains
  logical function f(x)
    real, intent(in) :: x
    f = .false.
  end function f
end
