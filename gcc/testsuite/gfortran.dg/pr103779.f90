! { dg-do compile }
! PR fortran/69636
! PR fortran/103779
! Contributed by G.Steinmetz

character(1,) function f() ! { dg-error "Expected initialization expression" }
  f = 'a'
end

character(1,kind=) function g() ! { dg-error "Expected initialization expression" }
  g = 'a'
end

character(1,n) function h() ! { dg-error "has not been declared" }
  h = 'a'
end

! { dg-prune-output "Bad kind expression" }
