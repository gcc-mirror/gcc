! { dg-do run }
! Test the fix for PR26891, in which an optional argument, whose actual
! is a missing dummy argument would cause a segfault.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
  logical :: back =.false.

! This was the case that would fail - PR case was an intrinsic call.
  if (scan ("A quick brown fox jumps over the lazy dog", "lazy", back) &
      .ne. myscan ("A quick brown fox jumps over the lazy dog", "lazy")) &
    call abort ()

! Check that the patch works with non-intrinsic functions.
  if (myscan ("A quick brown fox jumps over the lazy dog", "fox", back) &
      .ne. thyscan ("A quick brown fox jumps over the lazy dog", "fox")) &
    call abort ()

! Check that missing, optional character actual arguments are OK.
  if (scan ("A quick brown fox jumps over the lazy dog", "over", back) &
      .ne. thyscan ("A quick brown fox jumps over the lazy dog")) &
    call abort ()

contains
  integer function myscan (str, substr, back)
    character(*), intent(in) :: str, substr
    logical, optional, intent(in) :: back
    myscan = scan (str, substr, back)
  end function myscan

  integer function thyscan (str, substr, back)
    character(*), intent(in) :: str
    character(*), optional, intent(in) :: substr
    logical, optional, intent(in) :: back
    thyscan = isscan (str, substr, back)
  end function thyscan

  integer function isscan (str, substr, back)
    character(*), intent(in) :: str
    character(*), optional :: substr
    logical, optional, intent(in) :: back
    if (.not.present(substr)) then
      isscan = myscan (str, "over", back)
    else
      isscan = myscan (str, substr, back)
    end if
  end function isscan

end
