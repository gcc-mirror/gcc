! { dg-do run }
! Test the fix for PR55618, in which character scalar function arguments to
! elemental functions would gain an extra indirect reference thus causing
! failures in Vst17.f95, Vst 30.f95 and Vst31.f95 in the iso_varying_string
! testsuite, where elemental tests are done.
!
! Reported by Tobias Burnus  <burnus@gcc.gnu.org>
!
  integer, dimension (2) :: i = [1,2]
  integer :: j = 64
  character (len = 2) :: chr1 = "lm"
  character (len = 1), dimension (2) :: chr2 = ["r", "s"]
  if (any (foo (i, bar()) .ne. ["a", "b"])) STOP 1! This would fail
  if (any (foo (i, "xy") .ne. ["x", "y"])) STOP 2! OK - not a function
  if (any (foo (i, chr1) .ne. ["l", "m"])) STOP 3! ditto
  if (any (foo (i, char (j)) .ne. ["A", "B"])) STOP 4! This would fail
  if (any (foo (i, chr2) .ne. ["s", "u"])) STOP 5! OK - not a scalar
  if (any (foo (i, bar2()) .ne. ["e", "g"])) STOP 6! OK - not a scalar function
contains
  elemental character(len = 1) function foo (arg1, arg2)
    integer, intent (in) :: arg1
    character(len = *), intent (in) :: arg2
    if (len (arg2) > 1) then
      foo = arg2(arg1:arg1)
    else
      foo = char (ichar (arg2) + arg1)
    end if
  end function
  character(len = 2) function bar ()
    bar = "ab"
  end function
  function bar2 () result(res)
    character (len = 1), dimension(2) :: res
    res = ["d", "e"]
  end function
end
