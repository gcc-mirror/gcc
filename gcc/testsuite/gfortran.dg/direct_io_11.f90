! { dg-do run }
! PR42090 Problems reading partial records in formatted direct access files
! Test case from PR, prepared by Jerry DeLisle <jvdelisle@gcc.gnu.org>
program da_good_now
  implicit none
  real :: a, b

  a = 1.111111111
  b = 2.222222222

  open( 10, file = 't.dat', form = 'formatted', access = 'direct', recl = 12 )
  write( 10, rec = 1, fmt = '( f6.4, /, f6.4 )' ) a, b
  close( 10 )

  a = -1.0
  b = -1.0

  open( 10, file = 't.dat', form = 'formatted', access = 'direct', recl = 12 )

  read( 10, rec = 1, fmt = '( f6.4, /, f6.4 )' ) a, b
  !write( *, '( "partial record 1", t25, 2( f6.4, 1x ) )' ) a, b
  a = -1.0
  b = -1.0

  read( 10, rec = 1, fmt = '( f6.4 )' ) a, b
  !write( *, '( "partial record 2", t25, 2( f6.4, 1x ) )' ) a, b
  if (a /= 1.1111 .and. b /= 2.2222) STOP 1
  a = -1.0
  b = -1.0

  read( 10, rec = 1, fmt = '( f12.4, /, f12.4 )' ) a, b
  !write( *, '( "full record 1", t25, 2( f6.4, 1x ) )' ) a, b
  if (a /= 1.1111 .and. b /= 2.2222) STOP 2
  a = -1.0
  b = -1.0

  read( 10, rec = 1, fmt = '( f12.4 )' ) a, b
  !write( *, '( "full record 2", t25, 2( f6.4, 1x ) )' ) a, b
  if (a /= 1.1111 .and. b /= 2.2222) STOP 3
  a = -1.0
  b = -1.0

  read( 10, rec = 1, fmt = '( f6.4, 6x, /, f6.4, 6x )' ) a, b
  !write( *, '( "full record with 6x", t25, 2( f6.4, 1x ) )' ) a, b
  if (a /= 1.1111 .and. b /= 2.2222) STOP 4
  a = -1.0
  b = -1.0

  read( 10, rec = 1, fmt = '( f6.4 )' ) a
  read( 10, rec = 2, fmt = '( f6.4 )' ) b
  !write( *, '( "record at a time", t25, 2( f6.4, 1x ) )' ) a, b
  if (a /= 1.1111 .and. b /= 2.2222) STOP 5

  close( 10, status="delete")
end program da_good_now
