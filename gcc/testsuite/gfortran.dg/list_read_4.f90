! { dg-do run }
! Test of gfortran list directed read> check delimiters are correctly
! treated.  Written in f77 so that g77 will run for comparison.
!
! f , e and i edit reads are terminated separately by read_real.c
!
! PThomas                                                   Jan 2005
! BDavis        
      program list_read_4
      integer i(10),l(10),k,j 
      real x(10),y(10)
! expected results
      data y / 1.0,2.0,3.0,-1.0,-1.0,-1.0,4.0,4.0,99.0,99.0 /
      data l /1,2,3,-1,-1,-1,4,4,99,99/
! put them in a file
      open (10,status="scratch")
      write (10,*) " 1.0, 2.0 , 3.0,, 2* , 2*4.0 , 5*99.0"
      write (10,*) " 1.0e0, 2.0e0 , 3.0e0,, 2* , 2*4.0e0 , 5*99.0e0"
      write (10,*) " 1,   2 ,   3,,   2* , 2*4 ,   5*99"
      write (10,*) " 1,   2 ,   3,,   2* , 2*4 ,   5*99"
      rewind (10)
!
      do k = 1,10
        x(k) = -1.0
      enddo
      read (10,*,iostat=ier) x
      if (ier.ne.0) call abort
      do k = 1,10
         if (x(k).ne.y(k)) call abort
         x(k) = -1
      end do
      READ(10,*,iostat=ier) x
      if (ier.ne.0) call abort
      do k = 1,10
         if (x(k).ne.y(k)) call abort
         x(k) = -1
      end do
      READ(10,*,iostat=ier) x
      if (ier.ne.0) call abort
      do k = 1,10
         if (x(k).ne.y(k)) call abort
         x(k) = -1
      end do
! integer
      do k = 1,10
        i(k) = -1
      end do 
      READ(10,*,iostat=ier) (i(j),j=1,10)
      if (ier.ne.0) call abort
      do k = 1,10
         if (i(k).ne.y(k)) call abort
         i(k) = -1
      end do
      end
