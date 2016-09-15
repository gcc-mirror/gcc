! { dg-do compile }
! { dg-additional-options "-O2" }

program p
   integer, parameter :: n = 8
   integer :: i, z(n)
   z = [(i, i=1,n)]
   print *, z
end
subroutine s
   integer, parameter :: n = 8
   integer :: i, z(n)
   z = [(i, i=1,n)]
   print *, z
end
