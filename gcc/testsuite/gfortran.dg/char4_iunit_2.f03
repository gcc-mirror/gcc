! { dg-do run }
! PR37077 Implement Internal Unit I/O for character KIND=4
! Test case prepared by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
program char4_iunit_2
  implicit none
  integer, parameter :: k = 4
  character(kind=4,len=80) :: widestring, str_char4
  character(kind=1,len=80) :: skinnystring
  integer :: i,j
  real :: x
  character(9) :: str_default

  widestring = k_"12345 2.54360 hijklmnop qwertyuiopasdfg"
  skinnystring = "12345 2.54360 hijklmnop qwertyuiopasdfg"
  i = 77777
  x = 0.0
  str_default = "xxxxxxxxx"
  str_char4 = k_"xyzzy"
  read(widestring,'(i5,1x,f7.5,1x,a9,1x,a15)') i, x, str_default, str_char4
  if (i /= 12345 .or. (x - 2.5436001) > epsilon(x) .or. &
      str_default /= "hijklmnop" .or. str_char4 /= k_"qwertyuiopasdfg")&
      STOP 1
  i = 77777
  x = 0.0
  str_default = "xxxxxxxxx"
  str_char4 = k_"xyzzy"
  read(widestring,'(2x,i4,tl3,1x,f7.5,1x,a9,1x,a15)')i, x, str_default,&
       str_char4
  if (i /= 345 .or. (x - 52.542999) > epsilon(x) .or. &
      str_default /= "0 hijklmn" .or. str_char4 /= k_"p qwertyuiopasd")&
      STOP 2
  read(skinnystring,'(2x,i4,tl3,1x,f7.5,1x,a9,1x,a15)')i, x, str_default,&
   str_char4
  if (i /= 345 .or. (x - 52.542999) > epsilon(x) .or. &
      str_default /= "0 hijklmn" .or. str_char4 /= k_"p qwertyuiopasd")&
      STOP 3
  write(widestring,'(2x,i4,tl3,1x,f10.5,1x,a9,1x,a15)')i, x, str_default,&
   trim(str_char4)
  if (widestring .ne. k_"   3  52.54300 0 hijklmn p qwertyuiopasd") STOP 4
  write(skinnystring,'(2x,i4,tl3,1x,f10.5,1x,a9,1x,a15)')i, x, str_default,&
   trim(str_char4)
  if (skinnystring .ne. "   3  52.54300 0 hijklmn p qwertyuiopasd") STOP 5
  write(widestring,*)"test",i, x, str_default,&
   trim(str_char4)
  if (widestring .ne. &
    k_" test         345   52.5429993     0 hijklmnp qwertyuiopasd") STOP 6
end program char4_iunit_2
