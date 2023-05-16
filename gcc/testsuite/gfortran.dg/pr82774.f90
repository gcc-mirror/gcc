! { dg-do run }
!
! Contributed by Steve Kargl  <kargl@gcc.gnu.org>
!
program main
   implicit none
   type stuff
      character(:), allocatable :: key
   end type stuff
   type(stuff) nonsense, total
   nonsense = stuff('Xe')
   total = stuff(nonsense%key) ! trim nonsense%key made this work
   if (nonsense%key /= total%key) call abort
   if (len(total%key) /= 2) call abort
end program main
