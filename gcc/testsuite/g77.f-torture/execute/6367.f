      program testnl
      character*80 line
      dimension a(10),b(10)
      namelist /nl/ a
      data a / 10 * 0.0 /
      data b / 0.,  1.,  1.,  1.,  2.,  2.,  3.,  3.,  3.,  0. /
      data line /'&nl a(2) = 3*1.0, 2*2.0, 3*3.0 /'/
      open(1,status='scratch')
      write(1,'(a)') line
      rewind(1)
      read(1,nl)
      close(1)
      do i = 1, 10
         if (a(i) .ne. b(i)) call abort
      enddo
      end
