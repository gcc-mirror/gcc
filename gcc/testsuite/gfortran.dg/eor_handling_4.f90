! { dg-do run }
! PR 20092, 20131:  Handle end-of-record condition with pad=yes (default)
! for standard input.  This test case only really tests  anything if,
! by changing unit 5, you get to manipulate the standard input.
program main
  character(len=1) a(80)
  close(5)
  open(5,status="scratch")
  write(5,'(A)') 'one', 'two', 's'
  rewind(5)
  do i=1,4
     read(5,'(80a1)') a
     if (a(1) == 's') goto 100
  end do
  call abort
100 continue
end program main
