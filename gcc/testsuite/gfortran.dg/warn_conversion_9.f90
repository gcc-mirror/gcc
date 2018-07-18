! { dg-do compile }
! { dg-options "-Wconversion" }
! PR 78221 - used to give a spurious warning
complex, parameter :: i = (0.,1.)
complex :: t
t = (i)
end
