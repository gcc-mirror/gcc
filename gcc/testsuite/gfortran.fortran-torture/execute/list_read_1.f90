! pr 14942, list directed io
      program d
      implicit none
      integer i, j, m, n, nin, k
      real x(3,4)
      data x / 1,1,1,2,2,2,3,3,3,4,4,4 /
      real y(3,4)
      data y / 1,1,1,2,2,2,3,3,3,4,4,4 /
      logical debug ! set me true to see the output
      debug = .FALSE.
      nin = 1
      n = 4
      open(unit = nin)
      write(nin,*) n
      do I = 1,3
        write(nin,*)(x(i,j), j=1, n)
      end do
      m = 3
      n = 4
      write(nin,*) m,n
      do I = 1,3
         write(nin,*)(x(i,j), j=1, n)
      enddo
      close(nin)
! ok, the data file is written
      open(unit = nin)
      read(nin, fmt = *) n
      if (debug ) write(*,'(A,I2)') 'n = ', n
      do i = 1, 3
         do K = 1,n
             x(i,k) = -1
         enddo
         read(nin, fmt = *) (x(i,j), j=1, n)
         if (debug) write(*, *) (x(i,j), j=1, n)
          do K = 1,n
              if (x(i,k).ne.y(i,k)) call abort
          end do
      end do
      m = 0
      n = 0
      read(nin, fmt = *) m, n
      if (debug) write(*,'(A,I2,2X,A,I2)') 'm = ', m, 'n = ', n
      do i = 1, m
         do K = 1,n
             x(i,k) = -1
         enddo
         read(nin, fmt = *) (x(i,j), j=1, n)
         if (debug) write(*, *) (x(i,j), j=1, n)
         do K = 1,n
              if (x(i,k).ne.y(i,k)) call abort
         end do
      end do
      close(nin, status='delete')
      end program d
