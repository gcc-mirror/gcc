! { dg-do run }
! PR27138 Failure to advance line on bad list directed read.
! Submitted by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
      program test
      implicit none
      integer :: ntype = 55
      real    :: rtype
      complex :: ctype
      logical :: ltype
      OPEN (10, status="scratch")
      write(10,*) "aaaa aaaa aaaa aaaa"
      write(10,*) "bbbb bbbb bbbb bbbb"
      write(10,*) "cccc cccc cccc cccc"
      write(10,*) "dddd dddd dddd dddd"
      write(10,*) "  "
      write(10,*) "1234 5678 9012 3456"
      rewind(10)
      READ (10,*,END=77,ERR=77) ntype
      goto 99
   77 READ (10,*,END=78,ERR=78) rtype
      goto 99
   78 READ (10,*,END=79,ERR=79) ctype
      goto 99
   79 READ (10,*,END=80,ERR=80) ltype
      goto 99
   80 READ (10,*,END=99,ERR=99) ntype
      if (ntype.ne.1234) goto 99
      close(10)
      stop
   99 close(10)
      STOP 1
      end program test
