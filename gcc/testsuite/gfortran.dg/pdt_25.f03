! { dg-do run }
!
! Tests the fix for PR82978 in which all the parameterized string
! lengths with the same value of parameter 'k' had the same value
! regardless of the value of 'l'. In this testcase, the length for
! 'l' = 5 was taken.
!
! Contributed by Fritz Reese  <foreese@gcc.gnu.org>
!
  implicit none

  type :: pdt_t(k, l)
    integer, kind :: k
    integer, len :: l
    character(kind=k,len=l) :: chr
    integer :: i(l)
  end type

  type(pdt_t(1, 4))   :: x1
  type(pdt_t(1, 5))   :: x2
  type(pdt_t(4, 5))   :: x3

  call test (x1, 4)
  call test (x2, 5)

! Kind tests appear because of problem identified in comment #!
! due to Dominque d'Humieres  <dominiq@lps.ens.fr>

  if (kind (x2%chr) .ne. 1) STOP 1
  if (kind (x3%chr) .ne. 4) STOP 2

contains

  subroutine test (x, i)
    type(pdt_t(1, *)) :: x
    integer :: i

    if (x%l .ne. i) STOP 3
    if (len(x%chr) .ne. i) STOP 4
    if (size(x%i,1) .ne. i) STOP 5
  end subroutine

end
