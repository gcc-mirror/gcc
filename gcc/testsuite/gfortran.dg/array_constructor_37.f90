! { dg-do compile }
! Check the fix for PR47850, in which the argument of ANY, below, was not
! simplified, thereby causing an ICE.
!
! Contributed by Tobias Burnus  <burnus@gcc.gnu.org> but based on James van Buskirk's program in
! http://groups.google.com/group/comp.lang.fortran/browse_thread/thread/625faf82578e9af8
!
!
program Cindex
   implicit none
   integer,parameter :: SENSOR_CHANNEL(8) = &
      [10,12,17,20,22,30,33,34]
   integer,parameter :: NLTE_CHANNEL(3) = [20,22,34]
   integer,parameter :: N_NLTE_CHANNELS = size(NLTE_CHANNEL)
   integer,parameter :: N_CHANNELS = size(SENSOR_CHANNEL)
   integer i
   integer,parameter :: C_INDEX(8) = unpack( &
      vector = [(i,i=1,size(SENSOR_CHANNEL))], &
      mask = [(any(SENSOR_CHANNEL(i) == NLTE_CHANNEL), &
         i=lbound(SENSOR_CHANNEL,1),ubound(SENSOR_CHANNEL,1))], &
      field = 0)
   character(20) fmt

   write(fmt,'(a,i0,a)') '(a,t19,',size(SENSOR_CHANNEL),'(i3:","))'
   write(*,fmt) 'SENSOR_CHANNEL = ',SENSOR_CHANNEL
   write(fmt,'(a,i0,a)') '(a,t19,',size(NLTE_CHANNEL),'(i3:","))'
   write(*,fmt) 'NLTE_CHANNEL = ',NLTE_CHANNEL
   write(*,'(a,t19,i3)') 'N_NLTE_CHANNELS = ',N_NLTE_CHANNELS
   write(*,'(a,t19,i3)') 'N_CHANNELS = ',N_CHANNELS
   write(fmt,'(a,i0,a)') '(a,t19,',size(C_INDEX),'(i3:","))'
   write(*,fmt) 'C_INDEX = ',C_INDEX
end program Cindex
