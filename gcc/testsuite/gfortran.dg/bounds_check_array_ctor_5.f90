! { dg-do compile }
!
! PR 36112
! Check correct bounds-checking behavior for character-array-constructors.
! No need for -fbounds-check, enabled unconditionally.

  character(len=5) :: s = "hello"
  character(len=128) :: arr(3)
  arr = (/ "abc", "foo", s /) ! { dg-error "Different CHARACTER lengths" }
end
