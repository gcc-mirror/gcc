! { dg-do run }
! PR53029 Missed optimization, this test case took several seconds to
      program internalread
      implicit none
      integer m
      parameter(m=1000000)
      character value*10
      character(80) :: result
      integer i,j,intvalues(m)
      real :: start, finish
      intvalues = 33
      call cpu_time(start)
      do j=1,100
        write(value,'(i3,a5)') j," 5 69"
        read(value,*,end=20) intvalues
   20   write(result,*) (intvalues(i),i=2,4)
      if (result.ne.('           5          69          33')) STOP 1
      call cpu_time(finish)
      if ((finish-start).gt. 0.5) STOP 2
      enddo
      end program internalread
