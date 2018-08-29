! { dg-do run }
! { dg-options "-fbackslash" }

  implicit none
  integer :: i, j
  character(kind=4,len=5), dimension(3,3), parameter :: &
    p = reshape([4_" \xFF   ", 4_"\0    ", 4_" foo ", &
                 4_"\u1230\uD67Bde\U31DC8B30", 4_"     ", 4_"fa fe", &
                 4_"     ", 4_"foo  ", 4_"nul\0l"], [3,3])
       
  character(kind=4,len=5), dimension(3,3) :: m1
  character(kind=4,len=5), allocatable, dimension(:,:) :: m2

  if (kind (p) /= 4) STOP 1
  if (kind (m1) /= 4) STOP 2
  if (kind (m2) /= 4) STOP 3

  m1 = reshape (p, [3,3])

  allocate (m2(3,3))
  m2(:,:) = reshape (m1, [3,3])

  if (any (m1 /= p)) STOP 4
  if (any (m2 /= p)) STOP 5

  if (size (p) /= 9) STOP 6
  if (size (m1) /= 9) STOP 7
  if (size (m2) /= 9) STOP 8
  if (size (p,1) /= 3) STOP 9
  if (size (m1,1) /= 3) STOP 10
  if (size (m2,1) /= 3) STOP 11
  if (size (p,2) /= 3) STOP 12
  if (size (m1,2) /= 3) STOP 13
  if (size (m2,2) /= 3) STOP 14

  call check_shape (p, (/3,3/), 5)
  call check_shape (p, shape(p), 5)
  call check_shape (m1, (/3,3/), 5)
  call check_shape (m1, shape(m1), 5)
  call check_shape (m1, (/3,3/), 5)
  call check_shape (m1, shape(m1), 5)

  deallocate (m2)


  allocate (m2(3,4))
  m2 = reshape (m1, [3,4], p)
  if (any (m2(1:3,1:3) /= p)) STOP 15
  if (any (m2(1:3,4) /= m1(1:3,1))) STOP 16
  call check_shape (m2, (/3,4/), 5)
  deallocate (m2)

  allocate (m2(3,3))
  do i = 1, 3
    do j = 1, 3
      m2(i,j) = m1(i,j)
    end do
  end do

  m2 = transpose(m2)
  if (any(transpose(p) /= m2)) STOP 17
  if (any(transpose(m1) /= m2)) STOP 18
  if (any(transpose(m2) /= p)) STOP 19
  if (any(transpose(m2) /= m1)) STOP 20

  m1 = transpose(p)
  if (any(transpose(p) /= m2)) STOP 21
  if (any(m1 /= m2)) STOP 22
  if (any(transpose(m2) /= p)) STOP 23
  if (any(transpose(m2) /= transpose(m1))) STOP 24
  deallocate (m2)

  allocate (m2(3,3))
  m2 = p
  m1 = m2
  if (any (spread ( p, 1, 2) /= spread (m1, 1, 2))) STOP 25
  if (any (spread ( p, 1, 2) /= spread (m2, 1, 2))) STOP 26
  if (any (spread (m1, 1, 2) /= spread (m2, 1, 2))) STOP 27
  deallocate (m2)

  allocate (m2(3,3))
  m2 = p
  m1 = m2
  if (any (pack (p, p /= 4_"") /= [4_" \xFF   ", 4_"\0    ", 4_" foo ", &
                                   4_"\u1230\uD67Bde\U31DC8B30", 4_"fa fe", &
                                   4_"foo  ", 4_"nul\0l"])) STOP 28
  if (any (len_trim (pack (p, p /= 4_"")) /= [2,1,4,5,5,3,5])) STOP 29
  if (any (pack (m1, m1 /= 4_"") /= [4_" \xFF   ", 4_"\0    ", 4_" foo ", &
                                   4_"\u1230\uD67Bde\U31DC8B30", 4_"fa fe", &
                                   4_"foo  ", 4_"nul\0l"])) STOP 30
  if (any (len_trim (pack (m1, m1 /= 4_"")) /= [2,1,4,5,5,3,5])) STOP 31
  if (any (pack (m2, m2 /= 4_"") /= [4_" \xFF   ", 4_"\0    ", 4_" foo ", &
                                   4_"\u1230\uD67Bde\U31DC8B30", 4_"fa fe", &
                                   4_"foo  ", 4_"nul\0l"])) STOP 32
  if (any (len_trim (pack (m2, m2 /= 4_"")) /= [2,1,4,5,5,3,5])) STOP 33
  deallocate (m2)

  allocate (m2(1,7))
  m2 = reshape ([4_" \xFF   ", 4_"\0    ", 4_" foo ", &
                 4_"\u1230\uD67Bde\U31DC8B30", 4_"fa fe", &
                 4_"foo  ", 4_"nul\0l"], [1,7])
  m1 = p
  if (any (unpack(m2(1,:), p /= 4_"", 4_"     ") /= p)) STOP 34
  if (any (unpack(m2(1,:), m1 /= 4_"", 4_"     ") /= m1)) STOP 35
  deallocate (m2)

contains

  subroutine check_shape (array, res, l)
    character(kind=4,len=*), dimension(:,:) :: array
    integer, dimension(:) :: res
    integer :: l

    if (kind (array) /= 4) STOP 36
    if (len(array) /= l) STOP 37

    if (size (res) /= size (shape (array))) STOP 38
    if (any (shape (array) /= res)) STOP 39
  end subroutine check_shape

end
