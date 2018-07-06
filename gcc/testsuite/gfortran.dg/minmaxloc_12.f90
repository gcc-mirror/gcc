! { dg-do  run }
! Test compile-time simplification of minloc and maxloc with BACK argument
program main
  integer, parameter :: i1(*) = [ 1,2,3,1,2,3];
  integer, parameter :: d1 = minloc(i1,dim=1,back=.true.)
  integer, parameter :: d2 = minloc(i1,dim=1,back=.false.)
  integer, parameter :: d3 = maxloc(i1,dim=1,back=.true.)
  integer, parameter :: d4 = maxloc(i1,dim=1,back=.false.)
  integer, parameter :: i2(4,4) = reshape([1,2,1,2,2,3,3,2,3,4,4,3,4,5,5,4], &
       [4,4]);
  integer, parameter :: d5(2) = minloc(i2,back=.true.)
  integer, parameter :: d6(2) = maxloc(i2,back=.true.)
  integer, parameter :: d7(4) = minloc(i2,dim=1,back=.true.)
  integer, parameter :: d25(4) = minloc(i2,dim=2,mask=i2<2,back=.true.)
  integer, parameter :: d26(4) = maxloc(i2,dim=1,mask=i2<3,back=.true.)

  integer, parameter :: i3(4,4) = transpose(i2)
  integer, parameter :: d8(4) = minloc(i3,dim=2,back=.true.)
  integer, parameter :: i4(4,4) = reshape([1,2,1,2,2,1,2,1,1,2,1,2,2,1,2,1],&
       ([4,4]))
  integer, parameter :: d9(4) = minloc(i4,dim=1,mask=i4>1,back=.true.)

  integer, parameter :: d10(4) = maxloc(i4,dim=1,mask=i4>1,back=.true.)
  character(len=2), parameter :: c0(9) = ["aa", "bb", "aa", &
       "cc", "bb", "cc", "aa", "bb", "aa"]
  character(len=2), parameter :: c1 (3,3) = reshape(c0, [3,3]);
  integer, parameter :: d11(2) = minloc(c1,back=.true.)
  integer, parameter :: d12(2) = maxloc(c1,back=.true.)
  integer, parameter :: d13(2) = minloc(c1,mask=c1>"aa",back=.true.)
  integer, parameter :: d14(2) = maxloc(c1,mask=c1<"cc",back=.true.)
  integer, parameter :: d15(3) = minloc(c1,dim=1,back=.true.)
  integer, parameter :: d16(3) = maxloc(c1,dim=1,back=.true.)
  integer, parameter :: d17(3) = minloc(c1,dim=2,back=.true.)
  integer, parameter :: d18(3) = maxloc(c1,dim=2,back=.true.)
  integer, parameter :: d19    = minloc(c0,dim=1,back=.true.)
  integer, parameter :: d20    = maxloc(c0,dim=1,back=.true.)
  integer, parameter :: d21    = minloc(c0,dim=1,mask=c0>"aa",back=.true.)
  integer, parameter :: d22    = maxloc(c0,dim=1,mask=c0<"cc",back=.true.)
  integer, parameter :: d23(3) = minloc(c1,dim=2,mask=c1>"aa",back=.true.)
  integer, parameter :: d24(3) = maxloc(c1,dim=2,mask=c1<"cc",back=.true.)

  if (d1 /= 4) STOP 2078
  if (d2 /= 1) STOP 2079
  if (d3 /= 6) STOP 2080
  if (d4 /= 3) STOP 2081
  if (any (d5 /= [3,1])) STOP 2082
  if (any (d6 /= [3,4])) STOP 2083
  if (any (d7 /= [3,4,4,4])) STOP 2084
  if (any (d8 /= d7)) STOP 2085
  if (any (d9 /= [4,3,4,3])) STOP 2086
  if (any (d10 /= d9)) STOP 2087
  if (any(d11 /= [3,3])) STOP 2088
  if (any(d12 /= [3,2])) STOP 2089
  if (any(d13 /= [2,3])) STOP 2090
  if (any(d14 /= [2,3])) STOP 2091
  if (any(d15 /= [3,2,3])) STOP 2092
  if (any(d16 /= [2,3,2])) STOP 2093
  if (any(d17 /= [3,3,3])) STOP 2094
  if (any(d18 /= [2,3,2])) STOP 2095
  if (d19 /= 9) STOP 2096
  if (d20 /= 6) STOP 2097
  if (d21 /= 8 .or. d22 /= 8) STOP 2098
  if (any(d23 /= [2,3,2])) STOP 2099
  if (any(d24 /= 3)) STOP 2100
  if (any(d25 /= [1,0,1,0])) STOP 2101
  if (any(d26 /= [4,4,0,0])) STOP 2102
end program main
