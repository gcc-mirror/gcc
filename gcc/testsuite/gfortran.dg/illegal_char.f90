! { dg-do compile }
! PR 82372 - show hexcode of illegal, non-printable characters
program main
  tmp =È   1.0 ! { dg-error "Invalid character 0xC8" }
  print *,tmp
end
