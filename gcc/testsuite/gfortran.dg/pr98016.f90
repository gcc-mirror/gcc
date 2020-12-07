! { dg-do compile }
!
! Fix for PR98016 - Used to fail with Error: Variable ‘n’ cannot appear in the
! expression at (1) for line 16. Workaround was to declare y to be real.
!
! Posted by Juergen Reuter  <juergen.reuter@desy.de>
!
program is_it_valid
  dimension y(3)
  n=3
  y=func(1.0)
  print *, y
  stop
contains
  function func(x) result (y)
    dimension y(n)
    y=x
  end function
end
