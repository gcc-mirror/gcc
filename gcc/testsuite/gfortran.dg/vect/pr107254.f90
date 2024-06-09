subroutine dlartg( f, g, s, r )
  implicit none
  double precision :: f, g, r, s
  double precision :: d, p

  d = sqrt( f*f + g*g )
  p = 1.d0 / d
  if( abs( f ) > 1 ) then
     s = g*sign( p, f )
     r = sign( d, f )
  else
     s = g*sign( p, f )
     r = sign( d, f )
  end if
end subroutine

subroutine dhgeqz( n, h, t )
  implicit none
  integer            n
  double precision   h( n, * ), t( n, * )
  integer            jc
  double precision   c, s, temp, temp2, tempr
  temp2 = 10d0
  call dlartg( 10d0, temp2, s, tempr )
  c = 0.9d0
  s = 1.d0
  do jc = 1, n
     temp = c*h( 1, jc ) + s*h( 2, jc )
     h( 2, jc ) = -s*h( 1, jc ) + c*h( 2, jc )
     h( 1, jc ) = temp
     temp2 = c*t( 1, jc ) + s*t( 2, jc )
     t( 2, jc ) = -s*t( 1, jc ) + c*t( 2, jc )
     t( 1, jc ) = temp2
  enddo
end subroutine dhgeqz

program test
  implicit none
  double precision h(2,2), t(2,2)  
  h = 0
  t(1,1) = 1
  t(2,1) = 0
  t(1,2) = 0
  t(2,2) = 0
  call dhgeqz( 2, h, t )
  if (t(2,2).ne.0) STOP 1
end program test
