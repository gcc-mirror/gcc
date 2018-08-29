! { dg-do run }
! Test the fix for PR43180, in which patch which reduced the use of
! internal_pack/unpack messed up the passing of ru(1)%c as the actual
! argument at line 23 in this testcase.
!
! Contributed by Harald Anlauf <anlauf@gmx.de>
! further reduced by Tobias Burnus <burnus@gcc.gnu.org>
!
module mo_obs_rules
  type t_set
     integer :: use = 42
  end type t_set
  type t_rules
     character(len=40) :: comment
     type(t_set)       :: c (1)
  end type t_rules
  type (t_rules), save :: ru (1)
contains
  subroutine get_rule (c)
    type(t_set) :: c (:)
    ru(1)%c(:)%use = 99
    if (any (c(:)%use .ne. 42)) STOP 1
    call set_set_v (ru(1)%c, c)
    if (any (c(:)%use .ne. 99)) STOP 2
  contains
    subroutine set_set_v (src, dst)
      type(t_set), intent(in)    :: src(1)
      type(t_set), intent(inout) :: dst(1)
    if (any (src%use .ne. 99)) STOP 3
    if (any (dst%use .ne. 42)) STOP 4
      dst = src
    end subroutine set_set_v
  end subroutine get_rule
end module mo_obs_rules

program test
  use mo_obs_rules
  type(t_set) :: c (1)
  call get_rule (c)
end program test
