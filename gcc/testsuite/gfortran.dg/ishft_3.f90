! { dg-do compile }
program ishft_3
  integer i, j
  write(*,*) ishftc( 3, 2, 3 )
  write(*,*) ishftc( 3, 2, i )
  write(*,*) ishftc( 3, i, j )
  write(*,*) ishftc( 3, 128 )     ! { dg-error "exceeds BIT_SIZE of first" }
  write(*,*) ishftc( 3, 0, 128 )  ! { dg-error "exceeds BIT_SIZE of first" }
  write(*,*) ishftc( 3, 0, 0 )    ! { dg-error "Invalid third argument" }
  write(*,*) ishftc( 3, 3, 2 )    ! { dg-error "exceeds third argument" }
end program
