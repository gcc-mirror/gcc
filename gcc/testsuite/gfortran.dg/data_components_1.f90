! { dg-do compile }
! Check the fix for PR30879, in which the structure
! components in the DATA values would cause a syntax
! error.
!
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
!
  TYPE T1
   INTEGER :: I
  END TYPE T1

  TYPE(T1), PARAMETER :: D1=T1(2)
  TYPE(T1) :: D2(2)

  INTEGER :: a(2)

  DATA (a(i),i=1,D1%I) /D1%I*D1%I/

  DATA (D2(i),i=1,D1%I) /D1%I*T1(4)/

  print *, a
  print *, D2
  END
