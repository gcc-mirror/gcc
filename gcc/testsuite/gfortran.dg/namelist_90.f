! { dg-do run }
! PR71123 Namelist read failure on Windows 
      implicit none
      integer :: i, ierr
      real(8), dimension(30) :: senid, res
      character(2) :: crlf = char(13) // char(10)
      namelist /fith/ senid
      do i=1,30
         res(i) = i
      enddo
      senid = 99.0
      open(unit=7,file='test.out',form='formatted',
     *         status='new',action='readwrite', access='stream')
      write(7,'(a)') "&fith" // crlf
      write(7,'(a)') "senid=  1.0 , 2.0 , 3.0 , 4.0  , 5.0 ," // crlf
      write(7,'(a)') "6.0 , 7.0 ,  8.0 ,  9.0 ,  10.0 , 11.0 ," // crlf
      write(7,'(a)') "12.0 , 13.0 , 14.0 , 15.0 , 16.0 , 17.0 ," // crlf
      write(7,'(a)') "18.0 , 19.0 , 20.0 , 21.0 , 22.0 , 23.0 ," // crlf
      write(7,'(a)') "24.0 , 25.0 , 26.0 , 27.0 , 28.0 , 29.0 ," // crlf
      write(7,'(a)') "30.0 ," // crlf
      write(7,'(a)') "/" // crlf
      close(7)
      open(unit=7,file='test.out',form='formatted')
      read(7,nml=fith, iostat=ierr)
      close(7, status="delete")
      if (ierr.ne.0) STOP 1
      if (any(senid.ne.res)) STOP 2
      end
