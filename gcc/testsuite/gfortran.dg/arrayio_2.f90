! { dg-do run }
! PR 21875 : Test formatted input/output to/from character arrays.
! This test ckecks proper positioning and padding with trailing blanks
! after write operations.  Contributed by Paul Thomas.
     program arrayio_2
     implicit none
     integer :: i=2
     character(len=12), dimension(4,2)  :: r = "0123456789ab"
     character(len=80)                  :: f

     f = '("hello"/"world")'

     write(r(1:4,i-1), f)

     f = '("hello",t1,"HELLO",1x,"!"/"world",tl12,"WORLD")'

     write(r((i-1):(i+1),i), f)

     if ( r(1,1).ne.'hello       ' .or. &
	  r(2,1).ne.'world       ' .or. &
	  r(3,1).ne.'0123456789ab' .or. &
	  r(4,1).ne.'0123456789ab' .or. &
	  r(1,2).ne.'HELLO !     ' .or. &
	  r(2,2).ne.'WORLD       ' .or. &
	  r(3,2).ne.'0123456789ab' .or. &
	  r(4,2).ne.'0123456789ab') call abort()

     end program arrayio_2 
