! { dg-do run }
! { dg-options "-std=legacy" }
!
! This tests the fix for PR24276, which originated from the Loren P. Meissner example,
! Array_List.  The PR concerns dummy argument aliassing of components of arrays of derived
! types as arrays of the type of the component.  gfortran would compile and run this
! example but the stride used did not match the actual argument.  This test case exercises
! a procedure call (to foo2, below) that is identical to Array_List's.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>

program test_lex
  type :: dtype
    integer :: n
    character*5 :: word
  end type dtype

  type :: list
    type(dtype), dimension(4) :: list
    integer :: l = 4
  end type list
 
  type(list) :: table
  type(dtype) :: elist(2,2)

  table%list = (/dtype (1 , "one  "), dtype (2 , "two  "), dtype (3 , "three"), dtype (4 , "four ")/)

! Test 1D with assumed shape (original bug) and assumed size.
  call bar (table, 2, 4)
  if (any (table%list%word.ne.(/"one  ","i=  2","three","i=  4"/))) call abort ()

  elist = reshape (table%list, (/2,2/))

! Check 2D is OK with assumed shape and assumed size.
  call foo3 (elist%word, 1)
  call foo1 (elist%word, 3)
  if (any (elist%word.ne.reshape ((/"i=  1","i=  2","i=  3","i=  4"/), (/2,2/)))) call abort ()

contains

  subroutine bar (table, n, m)
    type(list) :: table
    integer n, m
    call foo1 (table%list(:table%l)%word, n)
    call foo2 (table%list(:table%l)%word, m)
  end subroutine bar

  subroutine foo1 (slist, i)
    character(*), dimension(*) :: slist
    integer i
    write (slist(i), '(2hi=,i3)') i
  end subroutine foo1

  subroutine foo2 (slist, i)
    character(5), dimension(:) :: slist
    integer i
    write (slist(i), '(2hi=,i3)') i
  end subroutine foo2

  subroutine foo3 (slist, i)
    character(5), dimension(:,:) :: slist
    integer i
    write (slist(1,1), '(2hi=,i3)') i
  end subroutine foo3

end program test_lex

