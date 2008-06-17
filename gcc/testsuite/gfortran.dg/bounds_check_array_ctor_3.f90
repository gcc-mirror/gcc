! { dg-do compile }
!
! PR 36112
! Check correct bounds-checking behaviour for character-array-constructors.
! This should not need any -fbounds-check and is enabled all the time.

  character(len=128) :: arr(2) = (/ "abc", "foobar" /) ! { dg-error "Different CHARACTER lengths" }
  arr = (/ "abc", "foobar" /) ! { dg-error "Different CHARACTER lengths" }
end
