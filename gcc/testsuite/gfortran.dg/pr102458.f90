! { dg-do compile }
! { dg-options "-fcoarray=lib" }
! PR fortran/102458 - standard intrinsics excluded in constant expressions

subroutine s1
  integer :: a(command_argument_count()) = 1 ! { dg-error "Automatic array" }
  print *, a
end

program p
  block
    integer :: a(get_team()) = 1 ! { dg-error "Automatic array" }
    print *, a
  end block
end

subroutine s2
  integer :: a(num_images()) = 1 ! { dg-error "Automatic array" }
  print *, a
end

function f()
  block
    integer :: a(team_number()) = 0 ! { dg-error "Automatic array" }
    a = 1
  end block
end

subroutine s3
  integer :: a(this_image()) = 1 ! { dg-error "Automatic array" }
  print *, a
end

subroutine s4
  integer, parameter :: n = 4
  integer, parameter :: x(transfer(n, n)) = 1 ! legal
  integer            :: y(transfer(n, n)) = 2 ! legal
  integer, parameter :: k = size (x)          ! ok
! integer, parameter :: m = size (y)          ! fails, tracked separately
  print *, k, x, y
  if (k /= size (y)) stop 1
end
