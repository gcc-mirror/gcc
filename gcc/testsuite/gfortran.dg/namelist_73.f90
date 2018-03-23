! { dg-do run }
!
! PR fortran/50109
!
! Contributed by Jim Hanson
!
      program namelist_test

      integer nfp
      namelist /indata/ nfp

      nfp = 99
      open(unit=4, status='scratch')
      write(4,'(a)') '$indata'
      write(4,'(a)') 'NFP = 5,'
      write(4,'(a)') "!  "
      write(4,'(a)') "! "
      write(4,'(a)') "!  "
      write(4,'(a)') '/'

      rewind(4)
      read (4,nml=indata)
      close(4)

!      write(*,*) nfp
      if (nfp /= 5) STOP 1

      end
