program alt_return
  implicit none
  
  call myproc (1, *10, 42)
20 continue
  call abort ()
10 continue
  call myproc(2, *20, 42)
  call myproc(3, *20, 42)
contains
subroutine myproc(n, *, i)
  integer n, i
  if (i .ne. 42) call abort ()
  if (n .eq. 1) return 1
  if (n .eq. 2) return
end subroutine
end program alt_return

