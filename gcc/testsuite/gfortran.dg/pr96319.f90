! { dg-do compile }
! { dg-options "-Wconversion -Wconversion-extra" }

program test
  LOGICAL(1) :: a
  logical(4) :: t = .true.
  logical(4) :: b
  logical(1) :: f = .false.
  a = t
  b = f
end program test

