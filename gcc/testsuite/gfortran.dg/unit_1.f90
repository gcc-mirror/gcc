! { dg-do run }
! PR40638 Run Time Error: Unit number in I/O statement too large
      program main
      integer(kind=2) ::  lun, anum
      integer(kind=1) ::  looney, bin
      lun  = 12
      anum = 5
      looney = 42
      bin = 23
      open (lun, status='scratch')
      write(lun,*) anum
      anum = 0
      rewind(lun)
      read (lun, *) anum
      if (anum.ne.5) call abort
      open (looney, status='scratch')
      write(looney,*)bin
      bin = 0
      rewind (looney)
      read (looney,*)bin
      if (bin.ne.23) call abort
      close (lun)
      close (looney)
      end
