! { dg-do run }
! PR69651 Usage of unitialized pointer io/list_read.c 
! Note: The uninitialized pointer was not the cause of the problem
!       observed with this test case. This tests the case with UTF-8
!       files. The large string test the realloc use in push_char4 of
!       list_read.c
program test
  implicit none
  integer :: i, j, k, ios
  integer, parameter :: big = 600
  real ::  r, s
  complex :: c, d
  character(kind=4,len=big) :: str1, str2, str3

  do i=1,big, 10
    do j = 0, 9
      k = i + j
      str2(k:k) = char(65+j)
    end do
  end do
  i = -5
  j = -6
  r = -3.14
  s = -2.71
  c = (-1.1,-2.2)
  d = (-3.3,-4.4)
  str3 = str2
  open(15, status='scratch', encoding="utf-8")
  write(15,*) "10  1!2"
  write(15,*) "  23.5! 34.5"
  write(15,*) "  (67.50,69.25)  (51.25,87.75)!"
  write(15,*) "  'abcdefgh!'", " ", str2
  rewind(15)
  str1 = 4_"candy"
  str2 = 4_"peppermint"
  read(15,*,iostat=ios) i, j
  if (ios.ne.5010) call abort
  read(15,*,iostat=ios) r, s
  if (ios.ne.5010) call abort
  read(15,*,iostat=ios) c, d
  if (ios.ne.5010) call abort
  read(15,*,iostat=ios) str1, str2
  if (ios.ne.0) call abort
  if (str1.ne.4_"abcdefgh!") call abort
  if (str2.ne.str3) call abort
  close(15)
end program
