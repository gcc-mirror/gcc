      subroutine mul66(rt,rtt,r)
      real*8 rt(6,6),r(6,6),rtt(6,6)
      do i=1,6
        do j=1,6
          do ia=1,6
            rtt(i,ia)=rt(i,j)*r(j,ia)+rtt(i,ia)
          end do
        end do
      end do
      end

      program test
      real*8 xj(6,6),w(6,6),w1(6,6)
      parameter(idump=0)
      integer i,j

      do i=1,6
        do j=1,6
          xj(i,j) = 0.0d0
          w1(i,j) = 0.0d0
          w(i,j) = i * 10.0d0 + j;
        end do
      end do

      xj(1,2) =  1.0d0
      xj(2,1) = -1.0d0
      xj(3,4) =  1.0d0
      xj(4,3) = -1.0d0
      xj(5,6) =  1.0d0
      xj(6,5) = -1.0d0

      call mul66(xj,w1,w)

      if (idump.ne.0) then
        write(6,*) 'w1 after call to mul66'
        do i = 1,6
          do j = 1,6
            write(6,'(D15.7)') w1(i,j)
          end do
        end do
      end if

      if (w1(1,1).ne.21.0d0) then
        call abort()
      end if

      end
