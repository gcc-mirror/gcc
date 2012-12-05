! { dg-do run }
! Test the fix for PR46897. First patch did not run this case correctly.
! Contributed by Tobias Burnus  <burnus@gcc.gnu.org>
!
module a_mod
  type :: a
    integer :: i = 99
  contains
     procedure :: a_ass
     generic :: assignment(=) => a_ass
  end type a

  type c
    type(a) :: ta
  end type c

  type :: b
    type(c) :: tc
  end type b

contains
  elemental subroutine a_ass(out, in)
    class(a), intent(INout) :: out
    type(a), intent(in)  :: in
      out%i = 2*in%i
  end subroutine a_ass
end module a_mod

program assign
  use a_mod
  type(b) :: tt
  type(b) :: tb1
  tt = tb1
  if (tt%tc%ta%i .ne. 198) call abort
end program assign
