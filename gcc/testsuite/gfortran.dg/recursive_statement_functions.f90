! { dg-do compile }
! PR20866 - A statement function cannot be recursive.
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
  INTEGER :: i, st1, st2, st3
  REAL :: x, z(2,2)
  character*8 :: ch
!
! Test check for recursion via other statement functions, string
! length references, function actual arguments and array index
! references.
  st1(i)=len(ch(st2(1):8))
  st2(i)=max (st3(1), 4)
  st3(i)=2 + cos (z(st1 (1), i)) ! { dg-error "is recursive" }
  write(6,*) st1(1)
  END

