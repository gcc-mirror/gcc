! { dg-do compile }
! { dg-options "-fno-automatic -frecursive" }
!
! Test case contributed by Mark Eggleston  <mark.eggleston@codethink.com>
!

program test
  ! do nothing
end program

! { dg-warning "Flag '-fno-automatic' overwrites '-frecursive'" "warning" { target *-*-* } 0 } 
