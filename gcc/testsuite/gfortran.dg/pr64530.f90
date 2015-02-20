! { dg-do run }

program bug
  ! Bug triggered with at least three elements
  integer, parameter :: asize = 3

  double precision,save :: ave(asize)
  double precision,save :: old(asize)
  double precision,save :: tmp(asize)

  ave(:) = 10.d0
  old(:) = 3.d0
  tmp(:) = 0.d0

  call buggy(2.d0,asize,ave,old,tmp)
  if (any (tmp(:) .ne. 3.5)) call abort
end

subroutine buggy(scale_factor, asize, ave, old, tmp)

  implicit none
  ! Args
  double precision scale_factor
  integer asize
  double precision ave(asize)
  double precision old(asize)
  double precision tmp(asize)

  ! Local 
  integer i

  do i = 1, asize
    tmp(i) = ave(i) - old(i)
    old(i) = ave(i)
    tmp(i) = tmp(i) / scale_factor
  end do

end subroutine buggy
