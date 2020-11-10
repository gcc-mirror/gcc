! { dg-do compile }

subroutine s
end subroutine

program pr95614
  common /c1/ s
  s = 9.0
end program
