! { dg-do run }
! PR 18958:  We used to segfault for eoshifting off the end of an array.
program main
  character(len=20) line
  write (line,'(I4)') eoshift((/1, 3/), 3)
end program main
