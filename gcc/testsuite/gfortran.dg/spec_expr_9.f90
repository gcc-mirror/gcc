! { dg-do compile }
!
! PR fortran/111781
! Used to fail with Error: Variable ‘n’ cannot appear in the
! expression at (1) for line 16.
!
program is_it_valid
  dimension y(3)
  integer :: n = 3
  interface
    function func(x)
      import
      dimension func(n)
    end function
  end interface
  y=func(1.0)
  print *, y
  stop
end
