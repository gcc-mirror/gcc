! { dg-do compile }
!
! PR fortran/34654
!
! Disallow unformatted write to internal unit.
! Test case was contributed by Joost VandeVondele.
!
implicit none
CHARACTER :: a(3)
WRITE(a) 0 ! { dg-error "Unformatted I/O not allowed with internal unit" }
END
