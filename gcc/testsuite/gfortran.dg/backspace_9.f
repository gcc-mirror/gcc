! { dg-do run { target fd_truncate } }
! PR32235 incorrectly position text file after backspace
! Test case from PR, prepared by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
      program main
      character*10 a
      ncards=2
      input=10
      write(10,"(a)") "One"
      write(10,"(a)") "Two"
      write(10,"(a)") "Three"
      rewind(10)
      read(input,1000)a
      read(input,1000)a

      call inlist(ncards)

      read(input,1000)a
      if (a.ne."Three") call abort
      close(10,status="delete")
      stop
 1000 format(a10)
 2000 format('read =',a10)
      end

      subroutine inlist(ncards)
      character*4 data(20)
      input=10
c
      if (ncards.eq.0) go to 20
      do 15 i=1,ncards
      backspace input
   15 continue
c
   20 continue
      kard  = 0
   30 read(input,1000,end=60)   data
   40 kard=kard + 1
   50 continue
      if ((kard .eq. 1) .and. (DATA(1) .ne. "One")) call abort
      if ((kard .eq. 2) .and. (DATA(1) .ne. "Two")) call abort
      if ((kard .eq. 3) .and. (DATA(1) .ne. "Thre")) call abort

      go to 30
   60 continue
      kard=kard - ncards + 1
      do 70   i=1,kard
      backspace input
   70 continue
c
      return
c
 1000 format (20a4)
 2020 format (8x,i15,8x,20a4)
c
      end
