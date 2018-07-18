subroutine par
  integer ix, jx

  !$acc parallel
  !$acc loop tile (*,*) ! { dg-error "not enough DO loops for tiled" }
  do ix = 1, 30
  end do

  !$acc loop tile (*,*)
  do ix = 1, 30
     do jx = 1, ix ! { dg-error "tiled loops don.t form rectangular" }
     end do
  end do

  !$acc loop tile (*)
  do ix = 1, 30
     do jx = 1, ix
     end do
  end do
  !$acc end parallel
end subroutine par
