! { dg-do run }
! PR 127731
! Pointer array function results with a span other than the element size,
! passed as actual arguments to various dummies and defined through.

module m
  implicit none
  type inner
    integer :: a
    real(8) :: b
  end type
  type t
    character(3) :: s
    integer :: i
    real(8) :: d
    logical :: l
    type(inner) :: in
  end type
  type(t), target :: ta(4)
  complex, target :: z2(2,3)
contains
  function gi() result(res)
    integer, pointer :: res(:)
    res => ta%i
  end function
  function gd() result(res)
    real(8), pointer :: res(:)
    res => ta%d
  end function
  function gl() result(res)
    logical, pointer :: res(:)
    res => ta%l
  end function
  function gin() result(res)
    type(inner), pointer :: res(:)
    res => ta%in
  end function
  function gs() result(res)
    character(3), pointer :: res(:)
    res => ta%s
  end function
  function gsn(m) result(res)
    integer :: m
    character(m), pointer :: res(:)
    res => ta%s
  end function
  function gsd() result(res)
    character(:), pointer :: res(:)
    res => ta%s
  end function
  function gsub() result(res)
    character(2), pointer :: res(:)
    res => ta%s(2:3)
  end function
  function g2() result(res)
    real, pointer :: res(:,:)
    res => z2%im
  end function
  integer function s_as (a)
    integer, intent(in) :: a(:)
    s_as = sum (a)
  end function
  integer function s_ex (a, n)
    integer, intent(in) :: n, a(n)
    s_ex = sum (a)
  end function
  integer function s_sz (a, n)
    integer, intent(in) :: n, a(*)
    s_sz = sum (a(1:n))
  end function
  integer function s_ct (a)
    integer, contiguous, intent(in) :: a(:)
    s_ct = sum (a)
  end function
  integer function s_tg (a)
    integer, target, intent(in) :: a(:)
    s_tg = sum (a)
  end function
  integer function s_pt (a)
    integer, pointer, intent(in) :: a(:)
    s_pt = sum (a)
  end function
  real(8) function s_in (a)
    type(inner), intent(in) :: a(:)
    s_in = sum (a%a) + sum (a%b)
  end function
  integer function cnt (a)
    logical, intent(in) :: a(:)
    cnt = count (a)
  end function
  real function s2 (a)
    real, intent(in) :: a(:,:)
    s2 = sum (a)
  end function
  function cat (a) result(r)
    character(*), intent(in) :: a(:)
    character(12) :: r
    r = a(1)//a(2)//a(3)//a(4)
  end function
  function s_ch (a) result(r)
    character(3), intent(in) :: a(4)
    character(12) :: r
    r = a(1)//a(2)//a(3)//a(4)
  end function
  subroutine w_ch (a)
    character(*), intent(inout) :: a(:)
    a = 'zz'
  end subroutine
  subroutine w_as (a)
    integer, intent(inout) :: a(:)
    a = a + 1
  end subroutine
  subroutine w_ex (a)
    integer, intent(inout) :: a(4)
    a = a + 1
  end subroutine
  subroutine w_out (a)
    real(8), intent(out) :: a(:)
    a = -5d0
  end subroutine
end module

program p
  use m
  implicit none
  integer :: k, i, j, ex(4)
  real :: rr(2,3)

  ex = [10, 20, 30, 40]
  do k = 1, 4
    ta(k) = t(achar(96+k)//'xy', 10*k, 100d0*k, mod(k,2) == 0, &
              inner(1000*k, -1d0*k))
  end do
  z2 = reshape ([((cmplx (10*i+j, -(10*i+j)), i=1,2), j=1,3)], [2,3])
  rr = -real (reshape ([((10*i+j, i=1,2), j=1,3)], [2,3]))

  if (any (gi() /= ex)) stop 1
  if (sum (gd()) /= 1000d0) stop 2
  if (count (gl()) /= 2) stop 3
  if (any (g2() /= rr)) stop 4
  if (any (pack (gi(), gl()) /= [20, 40])) stop 5
  if (any (gs() /= ['axy', 'bxy', 'cxy', 'dxy'])) stop 6

  if (s_as (gi()) /= 100) stop 10
  if (s_ex (gi(), 4) /= 100) stop 11
  if (s_sz (gi(), 4) /= 100) stop 12
  if (s_ct (gi()) /= 100) stop 13
  if (s_tg (gi()) /= 100) stop 14
  if (s_pt (gi()) /= 100) stop 15
  if (s_in (gin()) /= 9990d0) stop 16
  if (cnt (gl()) /= 2) stop 17
  if (s2 (g2()) /= sum (rr)) stop 18
  if (cat (gs()) /= 'axybxycxydxy') stop 19
  if (cat (gsub()) /= 'xyxyxyxy') stop 20
  k = 3
  if (cat (gsn(k)) /= 'axybxycxydxy') stop 21
  if (cat (gsd()) /= 'axybxycxydxy') stop 22
  if (s_ch (gsd()) /= 'axybxycxydxy') stop 23

  call w_as (gi())
  if (any (ta%i /= ex + 1)) stop 30
  call w_ex (gi())
  if (any (ta%i /= ex + 2)) stop 31
  call w_out (gd())
  if (any (ta%d /= -5d0)) stop 32
  gi() = [1, 2, 3, 4]
  call w_ch (gsd())
  if (any (ta%s /= 'zz') .or. any (ta%i /= [1, 2, 3, 4])) stop 34
  if (any (ta%i /= [1, 2, 3, 4])) stop 33

  associate (a => gi())
    if (any (a /= [1, 2, 3, 4])) stop 40
    if (s_as (a) /= 10) stop 41
    a = 7
  end associate
  if (any (ta%i /= 7)) stop 42
  associate (a => g2())
    if (s2 (a) /= sum (rr)) stop 43
  end associate
end program
