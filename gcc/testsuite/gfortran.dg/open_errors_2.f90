! { dg-do run }
! { dg-shouldfail "runtime error" }
! { dg-output "At line 13.*File already opened" }

! PR 65563 - this used to segfault for some versions.
  variable_1 = 0
  open(345,iostat=ios, form='unformatted')
  read(345, err=37, end=37) variable_1
  close(345)
  go to 38
37 continue
38 continue
  open(522, file="fort.345", form='unformatted')
  write(522) variable_1
  rewind(522)
  close(522)
end program
! { dg-final { remote_file build delete "fort.345" } }
