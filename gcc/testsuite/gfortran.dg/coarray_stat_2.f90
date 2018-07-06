! { dg-do run }
! { dg-options "-fcoarray=lib -lcaf_single" }
! { dg-additional-options "-latomic" { target libatomic_available } }
!
! Support for stat= in caf reference
!
program whitespace
  implicit none

  integer :: me[*],tmp,stat

  me = this_image()
  stat = 0

  sync all(stat = stat)

  if(stat /= 0) write(*,*) 'failure during sync'

  stat = 42

  tmp = me[num_images(),stat = stat]
  if(stat /= 0) STOP 1

end program whitespace
