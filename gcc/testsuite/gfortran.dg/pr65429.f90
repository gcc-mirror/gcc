! { dg-do run }
! PR fortran/65429
program foo

   implicit none

   character(*), parameter :: s(*) = [ character(5) :: 'abcde', 'fghij' ]
   character(*), parameter :: t(*) = [ character(31) :: ]
   character(*), parameter :: u(*) = [ 'qwerty', 'asdfgh', 'zxcvbn']
   character(*), parameter :: v(*) = ['','']

   if ((size(s) /= 2).or.(len(s)/=5)) call abort
   if ((size(t) /= 0).or.(len(t)/=31)) call abort
   if ((size(u) /= 3).or.(len(u)/=6)) call abort
   if ((size(v) /= 2).or.(len(v)/=0)) call abort
   if ((s(1)/='abcde').or.(s(2)/='fghij')) call abort
   if ((u(1)/='qwerty').or.(u(2)/='asdfgh').or.(u(3)/='zxcvbn')) call abort

end program foo
