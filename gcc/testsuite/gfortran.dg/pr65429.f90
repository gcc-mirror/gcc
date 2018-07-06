! { dg-do run }
! PR fortran/65429
program foo

   implicit none

   character(*), parameter :: s(*) = [ character(5) :: 'abcde', 'fghij' ]
   character(*), parameter :: t(*) = [ character(31) :: ]
   character(*), parameter :: u(*) = [ 'qwerty', 'asdfgh', 'zxcvbn']
   character(*), parameter :: v(*) = ['','']

   if ((size(s) /= 2).or.(len(s)/=5)) STOP 1
   if ((size(t) /= 0).or.(len(t)/=31)) STOP 2
   if ((size(u) /= 3).or.(len(u)/=6)) STOP 3
   if ((size(v) /= 2).or.(len(v)/=0)) STOP 4
   if ((s(1)/='abcde').or.(s(2)/='fghij')) STOP 5
   if ((u(1)/='qwerty').or.(u(2)/='asdfgh').or.(u(3)/='zxcvbn')) STOP 6

end program foo
