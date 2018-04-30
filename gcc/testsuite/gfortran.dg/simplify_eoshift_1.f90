! { dg-do  run }
program main
  implicit none
  integer :: i,n1, n2, n3
  integer, parameter :: a(3) = [1,2,3]
  integer, parameter :: b1(3) = eoshift(a,0)
  integer, parameter :: b2(3) = eoshift(a,1)
  integer, parameter :: b3(3) = eoshift(a,-2)
  integer, parameter :: b4(3) = eoshift(a,4,boundary=42)
  integer, parameter :: c(3,3) = reshape([(i,i=1,3*3)],shape(c))
  integer, parameter :: b5(3,3) = eoshift(c,shift=1,dim=1,boundary=33)
  integer, parameter :: b6(3,3) = eoshift(c,shift=2,dim=1)
  integer, parameter :: b7(3,3) = eoshift(c,shift=-1,dim=2)
  integer, parameter :: b8(3,3) = eoshift(c,shift=-1,dim=2,boundary=[-1,-2,-3])
  integer, parameter :: b9(3,3) = eoshift(c,shift=[-1,-1,-1])
  integer, parameter :: b10(3,3) = eoshift(c,shift=[-1,0,1]);
  integer, parameter :: b11(3,3) = eoshift(c,dim=2,shift=[-2,-1,1],boundary=42);
  integer, parameter :: b12(3,3) = eoshift(c,dim=1,shift=[-1,-2,0],boundary=[-3,-7,-9])
  real, parameter :: r(3,4,5) = reshape([(1.0*i**2,i=1,3*4*5)],shape(r))
  real, parameter :: q1(3,4,5) = eoshift(r,shift=1,dim=3)
  integer, parameter :: sh1(3,4) = reshape([-1,-2,0,3,2,5,6,-6,3,1,-1,-5],shape(sh1))
  real, parameter :: bnd1(3,4) = reshape([-1.,-2.,-3.,-4.,-5.,-6.,-7.,-8.,-9.,-10.,-11.,-12.],shape(bnd1))
  real, parameter :: q2(3,4,5) = eoshift(r,dim=3,shift=sh1)
  real, parameter :: q3(3,4,5) = eoshift(r,dim=3,shift=sh1,boundary=bnd1)
  complex(kind=8), parameter :: s(3,3) = reshape([(cmplx(i*i-5*i,-i+4,kind=8),i=1,9)],shape(s))
  complex(kind=8), parameter :: t(3,3) = eoshift(s,shift=4)
  character(len=3), parameter :: e(2,3,4) = reshape([(repeat(achar(i),3),i=iachar('a'),iachar('a')+2*3*4-1)], &
       shape(e))
  character(len=3) :: e2(2,3,4)
  character(len=3), parameter :: f1(2,3,4) = eoshift(e,1)
  character(len=3), parameter :: bnd2(2,4) = reshape([(repeat(achar(i),3),i=iachar('A'),iachar('A')+2*4-1)], &
       shape(bnd2))
  character(len=3), parameter :: f2(2,3,4) = eoshift(e,dim=2,shift=-1,boundary=bnd2);
  integer, parameter :: sh2(2,3) = reshape([1, -2, 0, 1, 2, -1, 2, 0],shape(sh2))
  character(len=3), parameter :: f3(2,3,4) = eoshift(e,dim=3,shift=sh2)
  integer, parameter :: empty(1:0) =[integer ::]
  integer, parameter :: empty2(1:0) = eoshift(empty,1)
  
  n1 = 1
  n2 = 2
  n3 = 3

  if (any(b1 /= a)) STOP 1
  if (any(b2 /= [2, 3, 0])) STOP 2
  if (any(b3 /= [0, 0, 1])) STOP 3
  if (any(b4 /= 42)) STOP 4
  if (any(eoshift(c,shift=1,dim=n1,boundary=33) /= b5)) STOP 5
  if (any(eoshift(c,shift=2,dim=1) /= b6)) STOP 6
  if (any(eoshift(c,shift=-1,dim=2) /= b7)) STOP 7
  if (any(eoshift(c,shift=-1,dim=n2,boundary=[-1,-2,-3]) /= b8)) STOP 8
  if (any(eoshift(c,shift=-1) /= b9)) STOP 9
  if (any(eoshift(r,shift=1,dim=n3) /= q1)) STOP 10
  if (any(b10 /= reshape([ 0, 1, 2, 4, 5, 6, 8, 9, 0],shape(b10)))) STOP 11
  if (any(b11 /= reshape([42, 42,  6, 42,  2,  9,  1,  5, 42],shape(b11)))) STOP 12
  if (any(b12 /= reshape([ -3,  1,  2, -7, -7,  4,  7,  8,  9],shape(b11)))) STOP 13
  if (any(q1 /= reshape([169.,196.,225.,256.,289.,324.,361.,400.,441.,484.,529.,576.,625.,&
          676.,729.,784.,841.,900.,961.,1024.,1089.,1156.,1225.,1296.,1369.,1444.,1521.,&
          1600.,1681.,1764.,1849.,1936.,2025.,2116.,2209.,2304.,2401.,2500.,2601.,2704.,&
          2809.,2916.,3025.,3136.,3249.,3364.,3481.,3600.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.],&
          shape(q1)))) STOP 14
  if (any(q2 /= reshape([0.,0.,9.,1600.,841.,0.,0.,0.,2025.,484.,0.,0.,1.,0.,225.,2704.,&
          1681.,0.,0.,0.,3249.,1156.,121.,0.,169.,4.,729.,0.,2809.,0.,0.,0.,0.,2116.,&
          529.,0.,625.,196.,1521.,0.,0.,0.,0.,0.,0.,3364.,1225.,0.,1369.,676.,2601.,&
          0.,0.,0.,0.,0.,0.,0.,2209.,0.],shape(q2)))) STOP 15
  if (any(q3 /= reshape([-1.,-2.,9.,1600.,841.,-6.,-7.,-8.,2025.,484.,-11.,-12.,1.,&
          -2.,225.,2704.,1681.,-6.,-7.,-8.,3249.,1156.,121.,-12.,169.,4.,729.,-4.,&
          2809.,-6.,-7.,-8.,-9.,2116.,529.,-12.,625.,196.,1521.,-4.,-5.,-6.,-7.,-8.,&
          -9.,3364.,1225.,-12.,1369.,676.,2601.,-4.,-5.,-6.,-7.,-8.,-9.,-10.,2209.,-12.],&
          shape(q3)))) STOP 16
  if (any(f1 /= reshape(["bbb","   ","ddd","   ","fff","   ","hhh","   ","jjj","   ","lll","   ",&
       "nnn","   ","ppp","   ","rrr","   ","ttt","   ","vvv","   ","xxx","   "], &
       shape(f1)))) STOP 17
  if (any(f2 /= reshape(["AAA","BBB","aaa","bbb","ccc","ddd","CCC","DDD","ggg","hhh","iii","jjj",&
       "EEE","FFF","mmm","nnn","ooo","ppp","GGG","HHH","sss","ttt","uuu","vvv"],shape(f2)))) STOP 18

  e2 = e
  if (any (f2 /= eoshift(e2,dim=2,shift=-1,boundary=bnd2))) STOP 19
  if (any (f3 /= reshape (["ggg","   ","ccc","jjj","qqq","   ","mmm","   ","iii","ppp",&
       "www","fff","sss","bbb","ooo","vvv","   ","lll","   ","hhh","uuu",&
       "   ","   ","rrr"], shape(f3)))) STOP 20
  if (size(empty) /=0) STOP 21
  if (any(t /= (0.0_8, 0.0_8))) STOP 22
end program main
