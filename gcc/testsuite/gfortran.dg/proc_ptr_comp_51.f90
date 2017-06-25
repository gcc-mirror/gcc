! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! PR 80983: [F03] memory leak when calling procedure-pointer component with allocatable result
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

program test
  implicit none

  type :: concrete_type
    procedure (alloc_integer), pointer, nopass :: alloc
  end type

  procedure (alloc_integer), pointer :: pp

  type(concrete_type) :: concrete

  print *, alloc_integer()     ! case #1: plain function

  pp => alloc_integer
  print *, pp()                ! case #2: procedure pointer

  concrete % alloc => alloc_integer
  print *, concrete % alloc()  ! case #3: procedure-pointer component

contains

   function alloc_integer() result(res)
      integer, allocatable :: res
      allocate(res, source=13)
   end function

end

! { dg-final { scan-tree-dump-times "__builtin_free" 3 "original" } }
