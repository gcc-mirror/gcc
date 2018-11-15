! { dg-do run }
! Various tests with findloc with character variables.
program main
  character(len=2) :: a(3,3), c(3,3), d(3,4)
  character(len=3) :: b(3,3)
  integer :: ret(2)
  integer :: i,j
  character(len=3) :: s
  logical :: lo
  logical, dimension(3,4) :: msk
  data a /"11", "21", "31", "12", "22", "32", "13", "23", "33" /
  data b /"11 ", "21 ", "31 ", "12 ", "22 ", "32 ", "13 ", "23 ", "33 " /
  if (any(findloc(a,"11 ") /= [1,1])) stop 1
  ret = findloc(b,"31")
  do j=1,3
     do i=1,3
        write(unit=s,fmt='(2I1," ")') i,j
        ret = findloc(b,s)
        if (b(ret(1),ret(2)) /= s) stop 2
     end do
  end do

  if (any(findloc(b(::2,::2),"13") /= [1,2])) stop 3

  do j=1,3
    do i=1,3
      write(unit=c(i,j),fmt='(I2)') 2+i-j
    end do
  end do

  if (any(findloc(c," 1") /= [1,2])) stop 4
  if (any(findloc(c," 1", back=.true.) /= [2,3])) stop 5
  if (any(findloc(c," 1", back=.true., mask=.false.) /= [0,0])) stop 6

  lo = .true.
  if (any(findloc(c," 2", dim=1) /= [1,2,3])) stop 7
  if (any(findloc(c," 2",dim=1,mask=lo) /= [1,2,3])) stop 8

  if (any(findloc(c," 2", dim=1,back=.true.) /= [1,2,3])) stop 9
  if (any(findloc(c," 2",dim=1,mask=lo,back=.true.) /= [1,2,3])) stop 10
  do j=1,4
     do i=1,3
        if (j<= i) then
           d(i,j) = "AA"
        else
           d(i,j) = "BB"
        end if
     end do
  end do
  print '(4A3)', transpose(d)
  if (any(findloc(d,"AA") /= [1,1])) stop 11
  if (any(findloc(d,"BB") /= [1,2])) stop 12
  msk = .true.
  if (any(findloc(d,"AA", mask=msk) /= [1,1])) stop 11
  if (any(findloc(d,"BB", mask=msk) /= [1,2])) stop 12
  if (any(findloc(d,"AA", dim=1) /= [1,2,3,0])) stop 13
  if (any(findloc(d,"BB", dim=1) /= [0,1,1,1])) stop 14
  if (any(findloc(d,"AA", dim=2) /= [1,1,1])) stop 15
  if (any(findloc(d,"BB", dim=2) /= [2,3,4])) stop 16
  if (any(findloc(d,"AA", dim=1,mask=msk) /= [1,2,3,0])) stop 17
  if (any(findloc(d,"BB", dim=1,mask=msk) /= [0,1,1,1])) stop 18
  if (any(findloc(d,"AA", dim=2,mask=msk) /= [1,1,1])) stop 19
  if (any(findloc(d,"BB", dim=2,mask=msk) /= [2,3,4])) stop 20

  if (any(findloc(d,"AA", dim=1, back=.true.) /= [3,3,3,0])) stop 21
  if (any(findloc(d,"AA", dim=1, back=.true., mask=msk) /= [3,3,3,0])) stop 22
  if (any(findloc(d,"BB", dim=2, back=.true.) /= [4,4,4])) stop 23
  if (any(findloc(d,"BB", dim=2, back=.true.,mask=msk) /= [4,4,4])) stop 24

  msk(1,:) = .false.
  print '(4L3)', transpose(msk)
  if (any(findloc(d,"AA", dim=1,mask=msk) /= [2,2,3,0])) stop 21
  if (any(findloc(d,"BB", dim=2,mask=msk) /= [0,3,4])) stop 22
  if (any(findloc(d,"AA", dim=2, mask=msk, back=.true.) /= [0,2,3])) stop 23
  if (any(findloc(d,"AA", dim=1, mask=msk, back=.true.) /= [3,3,3,0])) stop 24

end program main
