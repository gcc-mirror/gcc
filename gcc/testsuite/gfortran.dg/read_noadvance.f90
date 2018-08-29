! { dg-do run }
! pr24719, non-advancing read should read more than one line
! test contributed by jerry delisle <jvdelisle@gcc.gnu.org>
      implicit none
      character(1) :: chr
      character(20) :: correct = 'foo: bar 123abc'
      integer :: i
      open(unit = 11, status = "scratch", action="readwrite")
      write(11,'(a)') "foo: bar"
      write(11,'(a)') "123abc"
      rewind(11)
      i = 0
      do
        i = i + 1
10      read(unit = 11, fmt = '(a)', advance = 'no', end = 99, eor = 11) chr
        if (chr.ne.correct(i:i)) STOP 1
        cycle
11      continue
      end do
99    close(11)
      end
