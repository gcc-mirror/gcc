! { dg-do run }
! { dg-options "-pedantic" }
!  This test verifies the most basic sequential unformatted I/O
!  with convert="swap".
!  Adapted from seq_io.f.
!      write 3 records of various sizes
!      then read them back
program main
  implicit none
  integer size
  parameter(size=100)
  logical debug 
  data debug /.FALSE./
! set debug to true for help in debugging failures.
  integer m(2)
  integer n
  real r(size)
  integer i
  character(4) str

  m(1) = int(Z'11223344')
  m(2) = int(Z'55667788')
  n    = int(Z'77AABBCC')
  str = 'asdf'
  do i = 1,size
     r(i) = i
  end do
  open(9,form="unformatted",access="sequential",convert="swap") ! { dg-warning "Extension: CONVERT" }
  write(9) m  ! an array of 2
  write(9) n  ! an integer
  write(9) r  ! an array of reals
  write(9)str ! String
! zero all the results so we can compare after they are read back
  do i = 1,size
     r(i) = 0
  end do
  m(1) = 0
  m(2) = 0
  n = 0
  str = ' '
  
  rewind(9)
  read(9) m
  read(9) n
  read(9) r
  read(9) str
  !
  ! check results
  if (m(1).ne.int(Z'11223344')) then
     if (debug) then
        print '(A,Z8)','m(1) incorrect.  m(1) = ',m(1)
     else
        STOP 1
     endif
  endif
  
  if (m(2).ne.int(Z'55667788')) then
     if (debug) then
        print '(A,Z8)','m(2) incorrect.  m(2) = ',m(2)
     else
        STOP 2
     endif
  endif
  
  if (n.ne.int(Z'77AABBCC')) then
     if (debug) then
        print '(A,Z8)','n incorrect.  n = ',n
     else
        STOP 3
     endif
  endif
  
  do i = 1,size
     if (int(r(i)).ne.i) then
        if (debug) then
           print*,'element ',i,' was ',r(i),' should be ',i
        else
           STOP 4
        endif
     endif
  end do
  if (str .ne. 'asdf') then
     if (debug) then
        print *,'str incorrect, str = ', str
     else
        STOP 5
     endif
  end if
  ! use hexdump to look at the file "fort.9"
  if (debug) then
     close(9)
  else
     close(9,status='DELETE')
  endif
end program main
