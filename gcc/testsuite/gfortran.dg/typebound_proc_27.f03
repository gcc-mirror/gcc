! { dg-do run }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/47586
! Missing deep copy for data pointer returning functions when the type
! has allocatable components
!
! Original testcase by Thomas Henlich  <thenlich@users.sourceforge.net>
! Reduced by Tobias Burnus  <burnus@net-b.de>
!

module m
  type :: tx
    integer, dimension(:), allocatable :: i
  end type tx
  type proc_t
    procedure(find_x), nopass, pointer :: ppc => null()
   contains
    procedure, nopass :: tbp => find_x
  end type proc_t

contains

  function find_x(that)
    type(tx), target  :: that
    type(tx), pointer :: find_x
    find_x => that
  end function find_x

end module m

program prog

  use m

 block ! Start new scoping unit as PROGRAM implies SAVE
  type(tx) :: this
  type(tx), target :: that
  type(tx), pointer :: p

  type(proc_t) :: tab

  allocate(that%i(2))
  that%i = [3, 7]
  p => that
  this = that  ! (1) direct assignment: works (deep copy)
  that%i = [2, -5]
  !print *,this%i
  if(any (this%i /= [3, 7])) call abort()
  this = p     ! (2) using a pointer works as well
  that%i = [10, 1]
  !print *,this%i
  if(any (this%i /= [2, -5])) call abort()
  this = find_x(that)  ! (3) pointer function: used to fail (deep copy missing)
  that%i = [4, 6]
  !print *,this%i
  if(any (this%i /= [10, 1])) call abort()
  this = tab%tbp(that)  ! other case: typebound procedure
  that%i = [8, 9]
  !print *,this%i
  if(any (this%i /= [4, 6])) call abort()
  tab%ppc => find_x
  this = tab%ppc(that)  ! other case: procedure pointer component
  that%i = [-1, 2]
  !print *,this%i
  if(any (this%i /= [8, 9])) call abort()

 end block
end program prog

!
! We add another check for deep copy by looking at the dump.
! We use realloc on assignment here: if we do a deep copy  for the assignment
! to `this', we have a reallocation of `this%i'.
! Thus, the total number of malloc calls should be the number of assignment to
! `that%i' + the number of assignments to `this' + the number of allocate
! statements.
! It is assumed that if the number of allocate is right, the number of
! deep copies is right too.
! { dg-final { scan-tree-dump-times "__builtin_malloc" 15 "original" } }

!
! Realloc are only used for assignments to `that%i'.  Don't know why.
! { dg-final { scan-tree-dump-times "__builtin_realloc" 6 "original" } }
!

! No leak: Only assignments to `this' use malloc.  Assignments to `that%i'
! take the realloc path after the first assignment, so don't count as a malloc.
! { dg-final { scan-tree-dump-times "__builtin_free" 10 "original" } }
!

