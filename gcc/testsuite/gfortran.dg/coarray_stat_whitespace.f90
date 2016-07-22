! { dg-do compile }
! { dg-options "-fcoarray=lib" }
!
! Support for stat= in caf reference
!
program whitespace
  implicit none

  integer :: me[*],tmp,stat,i

  me = this_image()
  stat = 0
  i = 1

  sync all(stat = stat)

  if(stat /= 0) write(*,*) 'failure during sync'

  stat = 0

  if(me == 1) then
     tmp = me[num_images(),stat = stat]
     if(stat /= 0) write(*,*) 'failure in img:',me
  else if(me == 2) then
     tmp = me[i,stat=stat]
     if(stat /= 0) write(*,*) 'failure in img:',me
  endif

end program whitespace
