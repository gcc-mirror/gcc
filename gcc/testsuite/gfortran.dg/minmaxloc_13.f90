! { dg-do  run }
! Test run-time of MINLOC and MAXLOC with BACK
program main
  implicit none
  integer:: i1(6) 
  integer:: d1 
  integer:: d2 
  integer:: d3 
  integer:: d4 
  integer:: i2(4,4) 
  integer:: d5(2) 
  integer:: d6(2) 
  integer:: d7(4) 
  integer:: d25(4) 
  integer:: d26(4) 

  integer:: i3(4,4) 
  integer:: d8(4) 
  integer:: i4(4,4) 
  integer:: d9(4) 

  integer:: d10(4) 
  character(len=2) :: c0(9)
  character(len=2) :: c1(3,3)
  integer:: d11(2) 
  integer:: d12(2) 
  integer:: d13(2) 
  integer:: d14(2) 
  integer:: d15(3) 
  integer:: d16(3) 
  integer:: d17(3) 
  integer:: d18(3) 
  integer:: d19    
  integer:: d20    
  integer:: d21    
  integer:: d22    
  integer:: d23(3) 
  integer:: d24(3)

  i1 = [ 1,2,3,1,2,3];
  d1 = minloc(i1,dim=1,back=.true.)
  d2 = minloc(i1,dim=1,back=.false.)
  d3 = maxloc(i1,dim=1,back=.true.)
  d4 = maxloc(i1,dim=1,back=.false.)
  i2 = reshape([1,2,1,2,2,3,3,2,3,4,4,3,4,5,5,4], &
       [4,4]);
  d5 = minloc(i2,back=.true.)
  d6 = maxloc(i2,back=.true.)
  d7= minloc(i2,dim=1,back=.true.)
  d25 = minloc(i2,dim=2,mask=i2<2,back=.true.)
  d26 = maxloc(i2,dim=1,mask=i2<3,back=.true.)

  i3 = transpose(i2)
  d8 = minloc(i3,dim=2,back=.true.)
  i4 = reshape([1,2,1,2,2,1,2,1,1,2,1,2,2,1,2,1],&
       ([4,4]))
  d9 = minloc(i4,dim=1,mask=i4>1,back=.true.)

  d10 = maxloc(i4,dim=1,mask=i4>1,back=.true.)
  c0 = ["aa", "bb", "aa", &
       "cc", "bb", "cc", "aa", "bb", "aa"]
  c1 = reshape(c0, [3,3]);
  d11 = minloc(c1,back=.true.)
  d12 = maxloc(c1,back=.true.)
  d13 = minloc(c1,mask=c1>"aa",back=.true.)
  d14 = maxloc(c1,mask=c1<"cc",back=.true.)
  d15 = minloc(c1,dim=1,back=.true.)
  d16 = maxloc(c1,dim=1,back=.true.)
  d17 = minloc(c1,dim=2,back=.true.)
  d18 = maxloc(c1,dim=2,back=.true.)
  d19    = minloc(c0,dim=1,back=.true.)
  d20    = maxloc(c0,dim=1,back=.true.)
  d21    = minloc(c0,dim=1,mask=c0>"aa",back=.true.)
  d22    = maxloc(c0,dim=1,mask=c0<"cc",back=.true.)
  d23 = minloc(c1,dim=2,mask=c1>"aa",back=.true.)
  d24 = maxloc(c1,dim=2,mask=c1<"cc",back=.true.)

  if (d1 /= 4) STOP 2626
  if (d2 /= 1) STOP 2627
  if (d3 /= 6) STOP 2628
  if (d4 /= 3) STOP 2629
  if (any (d5 /= [3,1])) STOP 2630
  if (any (d6 /= [3,4])) STOP 2631
  if (any (d7 /= [3,4,4,4])) STOP 2632
  if (any (d8 /= d7)) STOP 2633
  if (any (d9 /= [4,3,4,3])) STOP 2634
  if (any (d10 /= d9)) STOP 2635
  if (any(d11 /= [3,3])) STOP 2636
  if (any(d12 /= [3,2])) STOP 2637
  if (any(d13 /= [2,3])) STOP 2638
  if (any(d14 /= [2,3])) STOP 2639
  if (any(d15 /= [3,2,3])) STOP 2640
  if (any(d16 /= [2,3,2])) STOP 2641
  if (any(d17 /= [3,3,3])) STOP 2642
  if (any(d18 /= [2,3,2])) STOP 2643
  if (d19 /= 9) STOP 2644
  if (d20 /= 6) STOP 2645
  if (d21 /= 8 .or. d22 /= 8) STOP 2646
  if (any(d23 /= [2,3,2])) STOP 2647
  if (any(d24 /= 3)) STOP 2648
  if (any(d25 /= [1,0,1,0])) STOP 2649
  if (any(d26 /= [4,4,0,0])) STOP 2650
end program
