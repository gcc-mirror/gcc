! { dg-do run }
! PR25463 Check that advance='no' works correctly.
! Derived from example given in PR by Thomas Koenig
! Contributed by Jerry DeLisle  <jvdelisle@gcc.gnu.org> 
program pr25463
  character(10) :: str
  write (10,'(A)',advance="no") 'ab'
  write (10,'(TL2,A)') 'c'
  rewind (10)
  read (10, '(a)') str
  if (str.ne.'abc') STOP 1
  close (10, status='delete')
end
