! { dg-do compile }
! PR 70260 - this used to ICE
! Original test case by Gernard Steinmetz
subroutine s (f)
   integer, external :: f, g
   integer :: h
   g = f(2) ! { dg-error "Illegal assignment to external procedure" }
   h = g(2)
end
