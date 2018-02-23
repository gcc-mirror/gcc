! PR libfortran/21471
! Testing POSITION="APPEND"
!
! { dg-do run }
      subroutine failed
        close (10,status='delete')
        STOP 1
      end subroutine failed

      integer,parameter :: n = 13
      integer :: i, j, error

      open (10, file='foo')
      close (10)

      do i = 1, n
        open (10, file='foo',position='append')
        write (10,*) i
        close (10)
      end do

      open (10,file='foo',status='old')
      error = 0
      i = -1
      do while (error == 0)
        i = i + 1
        read (10,*,iostat=error) j
        if (error == 0) then
          if (i + 1 /= j) call failed
        end if
        if (i > n + 1) call failed
      end do
      if (i /= n) call failed
      close (10,status='delete')
      end

