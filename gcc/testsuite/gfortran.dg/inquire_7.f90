! { dg-do run }
! pr 19647 / segfault on inquire(..pad=..)
!   Thomas.Koenig@online.de
!   bdavis9659@comcast.net
       program main
       character(len=10) delim
! quote
       open(10,delim='quote',status='SCRATCH')
       inquire(10,delim=delim)
       close(10)
       if (delim .ne. 'QUOTE') call abort
! apostrophe
       open(10,delim='apostrophe',status='SCRATCH')
       inquire(10,delim=delim)
       close(10)
       if (delim .ne. 'APOSTROPHE') call abort
! none
       open(10,status='SCRATCH')
       inquire(10,delim=delim)
       close(10)
       if (delim .ne. 'NONE') call abort
! undefined
       open(10,form='UNFORMATTED',status='SCRATCH')
       inquire(10,delim=delim)
       close(10)
       if (delim .ne. 'UNDEFINED') call abort
       end program main
