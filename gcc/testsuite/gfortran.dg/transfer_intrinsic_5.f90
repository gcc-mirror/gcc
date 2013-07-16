! { dg-do run }
!
! PR fortran/56615
!
! Contributed by  Harald Anlauf
!
!
program gfcbug
  implicit none
  integer, parameter             :: n = 8
  integer                        :: i
  character(len=1), dimension(n) :: a, b
  character(len=n)               :: s, t
  character(len=n/2)             :: u

  do i = 1, n
     a(i) = achar (i-1 + iachar("a"))
  end do
!  print *, "# Forward:"
!  print *, "a=", a
  s = transfer (a, s)
!  print *, "s=", s
  call cmp (a, s)
!  print *, "  stride = +2:"
  do i = 1, n/2
     u(i:i) = a(2*i-1)
  end do
!  print *, "u=", u
  call cmp (a(1:n:2), u)
!  print *
!  print *, "# Backward:"
  b = a(n:1:-1)
!  print *, "b=", b
  t = transfer (b, t)
!  print *, "t=", t
  call cmp (b, t)
!  print *, "  stride = -1:"
  call cmp (a(n:1:-1), t)
contains
  subroutine cmp (b, s)
    character(len=1), dimension(:), intent(in) :: b
    character(len=*),               intent(in) :: s
    character(len=size(b))                     :: c
    c = transfer (b, c)
    if (c /= s) then
      print *, "c=", c, "    ", merge ("  ok","BUG!", c == s)
      call abort ()
    end if
  end subroutine cmp
end program gfcbug
