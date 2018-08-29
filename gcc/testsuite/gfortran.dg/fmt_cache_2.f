! { dg-do run }
! PR42742 Handle very large format strings correctly
! Test derived from example developed by Manfred Schwarb.
      character(12) bufarr(74)
      character(74*13+30) fmtstr,fmtstr2
      character(1) delim
      integer i,j,dat(5),pindx, loopcounter
      character(983) big_string ! any less and this test fails.

      do i=1,74
        write(bufarr(i),'(i12)') i
      enddo

      delim=" "
      dat(1)=2009
      dat(2)=10
      dat(3)=31
      dat(4)=3
      dat(5)=0
      fmtstr="(i2,i6,4(a1,i2.2)"
      open(10, status="scratch")
      do j=1,74
        fmtstr=fmtstr(1:len_trim(fmtstr))//",a1,a12"
        fmtstr2=fmtstr(1:len_trim(fmtstr))//")"
c        write(0,*) "interation ",j,": ",len_trim(fmtstr2)
        do i=1,10
          write(10,fmtstr2)
     &           i,dat(1),"-",dat(2),"-",dat(3),
     &           delim,dat(4),":",dat(5),
     &           (delim,bufarr(pindx),pindx=1,j)
        enddo
        loopcounter = j
      enddo
      close(10)
      if (loopcounter /= 74) STOP 1
      end
