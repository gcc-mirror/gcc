! { dg-compile }
! { dg-options "-Wsurprising" }

! There should be no surprising warnings

program p
  Integer :: nargs
  intrinsic :: command_argument_count
  nargs = command_argument_count()
end

