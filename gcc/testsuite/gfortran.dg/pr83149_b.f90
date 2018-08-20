! Compiled with pr83149_a.f90
! { dg-do run }
! { dg-compile-aux-modules "pr83149_a.f90" }
! { dg-additional-sources pr83149_a.f90 }
!
! Contributed by Neil Carlson  <neil.n.carlson@gmail.com>
!
  use mod
  string = 'fubar'
  select case (get_string())
    case ('fubar')
    case default
      stop 1
  end select
end
