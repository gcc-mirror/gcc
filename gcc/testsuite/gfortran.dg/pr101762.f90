! { dg-do compile }
! PR fortran/101762 - ICE on non-constant pointer initialization targets
! Contributed by G.Steinmetz

program p
  integer,      target  :: a(3) = [7, 8, 9]
  integer,      pointer :: x    => a(3)
  integer,      pointer :: y    => a(n())  ! { dg-error "constant expression" }
  integer,      pointer :: z(:) => a(:n()) ! { dg-error "constant expression" }
  character(7), target  :: c    = "abcdefg"
  character(3), pointer :: c0   => c(2:4)
  character(3), pointer :: c1   => c(m():) ! { dg-error "constant expression" }
  character(3), pointer :: c2   => c(:m()) ! { dg-error "constant expression" }
  print *, x, y
contains
  pure integer function k ()
    k = 2
  end function k
  subroutine s ()
    integer, pointer :: yy => a(k()) ! { dg-error "constant expression" }
    print *, yy
  end subroutine s
end
