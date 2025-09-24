! { dg-do run }
! PR fortran/108581 - issues with rank-2 deferred-length character arrays
! PR fortran/121939 - ICE in gfc_conv_string_parameter

program p
  call pr108581
  call test2
end

! Derived from original testcase
subroutine pr108581
  integer, parameter :: xmin = 0, xmax = 0
  integer, parameter :: ymin = 0, ymax = 1
  integer, parameter :: l = 2
  integer            :: x, y
  character(8)       :: line1, line2, line3
  character(*),   parameter :: expect(ymin:ymax) = ['A.','B*']
  character(len=:), pointer :: a(:,:) => NULL()

  allocate (character(len=l) :: a(xmin:xmax, ymin:ymax))
  a(xmin:xmax, ymin) = expect(ymin)
  a(xmin:xmax, ymax) = expect(ymax)

  do y = ymin, ymax
     write(line1,'(4A)') (a(x, y), x = xmin, xmax)
     write(line2,'(4A)')  a(xmin:xmax, y)
     write(line3,'(4A)')  a(    :    , y)
     if (line1 /= expect(y) .or. &
         line2 /= expect(y) .or. &
         line3 /= expect(y)      ) then
        write(*,*) (a(x, y), x = xmin, xmax)
        write(*,*)  a(xmin:xmax, y)
        write(*,*)  a(    :    , y)
        stop 1 + y
     end if
  enddo
  call chk (a)
  deallocate (a)
contains
  subroutine chk (z)
    character(len=:), pointer :: z(:,:)
    integer :: y
    do y = lbound(z,2), ubound (z,2)
       write(line2,'(4A)')  z(xmin:xmax, y)
       write(line3,'(4A)')  z(    :    , y)
       if (line2 /= expect(y) .or. &
           line3 /= expect(y)      ) then
          write(*,*) z(xmin:xmax, y)
          write(*,*) z(    :    , y)
          stop 5 + y
       end if
    enddo
  end subroutine chk
end

! Exercise character kinds, strides, ...
subroutine test2
  implicit none
  integer, parameter :: l = 3
  integer            :: i

  character(len=l,kind=1), parameter :: str1(*) = &
       [   "123",   "456",   "789",   "0AB" ]
  character(len=l,kind=4), parameter :: str4(*) = &
       [ 4_"123", 4_"456", 4_"789", 4_"0AB" ]

  character(len=l,kind=1), parameter :: str2(*,*) = &
       reshape ([(str1(i),str1(5-i),i=1,4)], shape=[2,4])
  character(len=l,kind=4), parameter :: str5(*,*) = &
       reshape ([(str4(i),str4(5-i),i=1,4)], shape=[2,4])

  character(len=l,kind=1), pointer :: a(:,:) => NULL(), e(:,:) => NULL()
  character(len=:,kind=1), pointer :: b(:,:) => NULL(), f(:,:) => NULL()
  character(len=l,kind=4), pointer :: c(:,:) => NULL(), g(:,:) => NULL()
  character(len=:,kind=4), pointer :: d(:,:) => NULL(), h(:,:) => NULL()

  character(len=16) :: s0, s1, s2, s3, s4

  ! Simple case: shape=[1,4]
  allocate (a, source = reshape (str1,[1,size(str1)]))
  allocate (b, source = reshape (str1,[1,size(str1)]))
  allocate (c, source = reshape (str4,[1,size(str4)]))
  allocate (d, source = c)      ! fixed with pr121939
! d => c
  ! Positive non-unit stride
  s0 = concat (str1(1::2))
  write(s1,'(4A)') a(1,1::2)
  write(s2,'(4A)') b(1,1::2)
  write(s3,'(4A)') c(1,1::2)
  write(s4,'(4A)') d(1,1::2)
! print *, s0, s1, s2, s3, s4
  if (s1 /= s0) stop 11
  if (s2 /= s0) stop 12
  if (s3 /= s0) stop 13
  if (s4 /= s0) stop 14
  s0 = concat (str1(2::2))
  write(s1,'(4A)') a(1,2::2)
  write(s2,'(4A)') b(1,2::2)
  write(s3,'(4A)') c(1,2::2)
  write(s4,'(4A)') d(1,2::2)
! print *, s0, s1, s2, s3, s4
  if (s1 /= s0) stop 15
  if (s2 /= s0) stop 16
  if (s3 /= s0) stop 17
  if (s4 /= s0) stop 18

  ! Negative non-unit stride
  s0 = concat (str1(3:1:-2))
  write(s1,'(4A)') a(1,3:1:-2)
  write(s2,'(4A)') b(1,3:1:-2)
  write(s3,'(4A)') c(1,3:1:-2)
  write(s4,'(4A)') d(1,3:1:-2)
! print *, s0, s1, s2, s3, s4
  if (s1 /= s0) stop 21
  if (s2 /= s0) stop 22
  if (s3 /= s0) stop 23
  if (s4 /= s0) stop 24
  s0 = concat (str1(4:1:-2))
  write(s1,'(4A)') a(1,4:1:-2)
  write(s2,'(4A)') b(1,4:1:-2)
  write(s3,'(4A)') c(1,4:1:-2)
  write(s4,'(4A)') d(1,4:1:-2)
! print *, s0, s1, s2, s3, s4
  if (s1 /= s0) stop 25
  if (s2 /= s0) stop 26
  if (s3 /= s0) stop 27
  if (s4 /= s0) stop 28
  deallocate (a,b,c,d)

  ! More complex cases with shape=[2,4]
  allocate (e, source = reshape (str2,[2,size(str2,2)]))
  allocate (f, source = reshape (str2,[2,size(str2,2)]))
  allocate (g, source = reshape (str5,[2,size(str5,2)]))
  allocate (h, source = reshape (str5,[2,size(str5,2)])) ! fixed with pr121939
! h => g
  s0 = concat (str2(1,3:1:-2))
  write(s1,'(4A)') e(1,3:1:-2)
  write(s2,'(4A)') f(1,3:1:-2)
  write(s3,'(4A)') g(1,3:1:-2)
  write(s4,'(4A)') h(1,3:1:-2)
! print *, s0, s1, s2, s3, s4
  if (s1 /= s0) stop 31
  if (s2 /= s0) stop 32
  if (s3 /= s0) stop 33
  if (s4 /= s0) stop 34
  s0 = concat (str2(1,4:1:-2))
  write(s1,'(4A)') e(1,4:1:-2)
  write(s2,'(4A)') f(1,4:1:-2)
  write(s3,'(4A)') g(1,4:1:-2)
  write(s4,'(4A)') h(1,4:1:-2)
! print *, s0, s1, s2, s3, s4
  if (s1 /= s0) stop 35
  if (s2 /= s0) stop 36
  if (s3 /= s0) stop 37
  if (s4 /= s0) stop 38

  s0 = concat (str2(2,3:1:-2))
  write(s1,'(4A)') e(2,3:1:-2)
  write(s2,'(4A)') f(2,3:1:-2)
  write(s3,'(4A)') g(2,3:1:-2)
  write(s4,'(4A)') h(2,3:1:-2)
! print *, s0, s1, s2, s3, s4
  if (s1 /= s0) stop 41
  if (s2 /= s0) stop 42
  if (s3 /= s0) stop 43
  if (s4 /= s0) stop 44
  s0 = concat (str2(2,4:1:-2))
  write(s1,'(4A)') e(2,4:1:-2)
  write(s2,'(4A)') f(2,4:1:-2)
  write(s3,'(4A)') g(2,4:1:-2)
  write(s4,'(4A)') h(2,4:1:-2)
! print *, s0, s1, s2, s3, s4
  if (s1 /= s0) stop 45
  if (s2 /= s0) stop 46
  if (s3 /= s0) stop 47
  if (s4 /= s0) stop 48

  ! Check pointer association with negative stride
  a => e(2:1:-1,4:1:-1)
  b => f(2:1:-1,4:1:-1)
  c => g(2:1:-1,4:1:-1)
  d => h(2:1:-1,4:1:-1)

  s0 = concat (str2(2,4:1:-2))
  write(s1,'(4A)') a(1,1::2)
  write(s2,'(4A)') b(1,1::2)
  write(s3,'(4A)') c(1,1::2)
  write(s4,'(4A)') d(1,1::2)
! print *, s0, s1, s2, s3, s4
  if (s1 /= s0) stop 51
  if (s2 /= s0) stop 52
  if (s3 /= s0) stop 53
  if (s4 /= s0) stop 54
  s0 = concat (str2(2,3:1:-2))
  write(s1,'(4A)') a(1,2::2)
  write(s2,'(4A)') b(1,2::2)
  write(s3,'(4A)') c(1,2::2)
  write(s4,'(4A)') d(1,2::2)
! print *, s0, s1, s2, s3, s4
  if (s1 /= s0) stop 55
  if (s2 /= s0) stop 56
  if (s3 /= s0) stop 57
  if (s4 /= s0) stop 58

  s0 = concat (str2(1,4:1:-2))
  write(s1,'(4A)') a(2,1::2)
  write(s2,'(4A)') b(2,1::2)
  write(s3,'(4A)') c(2,1::2)
  write(s4,'(4A)') d(2,1::2)
! print *, s0, s1, s2, s3, s4
  if (s1 /= s0) stop 61
  if (s2 /= s0) stop 62
  if (s3 /= s0) stop 63
  if (s4 /= s0) stop 64
  s0 = concat (str2(1,3:1:-2))
  write(s1,'(4A)') a(2,2::2)
  write(s2,'(4A)') b(2,2::2)
  write(s3,'(4A)') c(2,2::2)
  write(s4,'(4A)') d(2,2::2)
! print *, s0, s1, s2, s3, s4
  if (s1 /= s0) stop 65
  if (s2 /= s0) stop 66
  if (s3 /= s0) stop 67
  if (s4 /= s0) stop 68
  deallocate (e,f,g,h)

contains

  ! Helper function to concatenate string array to scalar string
  function concat (s)
    character(len=:), allocatable :: concat
    character(len=*), intent(in)  :: s(:)
    integer :: i, l, n
    n = size (s)
    l = len  (s)
    allocate (character(len=l*n) :: concat)
    do i = 1, n
       concat(1+(i-1)*l:i*l) = s(i)
    end do
  end function concat
end
