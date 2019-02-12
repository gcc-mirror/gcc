! { dg-do compile }
! PR 71860 - this used to ICE
! Original test case by Gerhard Steinmetz
program p
   class(*), pointer :: z
   z => null(z)
end
