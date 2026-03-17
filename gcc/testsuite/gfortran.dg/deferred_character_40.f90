! { dg-do run }
! { dg-options "-O2 -fno-automatic" }
!
! PR fortran/78187 - deferred-length character results and -fno-automatic

program pr78187
  implicit none
  character(len=:), allocatable :: a, b, c, d
  save :: b,c,d
  c = scalar()
  d = scalar_with_result()
  if (len (c) /= 6) stop 11
  if (len (d) /= 6) stop 12
  deallocate (c,d)
  c = pass_scalar()
  d = pass_scalar_with_result()
  if (len (c) /= 6) stop 13
  if (len (d) /= 6) stop 14
  deallocate (c,d)
  call sub (c,d)
  if (len (c) /= 6) stop 15
  if (len (d) /= 6) stop 16
  deallocate (c,d)
  call sub2 (c,d)
  if (len (c) /= 3) stop 17
  if (len (d) /= 6) stop 18
  deallocate (c,d)
  a = concat ("abc","def")
  b = concat ("abc","def")
  if (len (a) /= 6) stop 19
  if (len (b) /= 6) stop 20
contains
  function concat(a, b) result(c)
    character(len=*), intent(in) :: a, b
    character(len=:), allocatable :: c
    c = trim(a)//trim(b)
  end function concat

  function scalar()
    character(len=:), allocatable :: scalar
    scalar = "abcdef"
    if (len(scalar) /= 6) stop 1
  end function scalar

  function scalar_with_result() result(s)
    character(len=:), allocatable :: s
    s = "abcdef"
    if (len(s) /= 6) stop 2
  end function scalar_with_result

  function pass_scalar ()
    character(len=:), allocatable :: pass_scalar
    pass_scalar = scalar ()
    if (len(pass_scalar) /= 6) stop 3
  end function pass_scalar

  function pass_scalar_with_result() result(s)
    character(len=:), allocatable :: s
    s = scalar_with_result ()
    if (len(s) /= 6) stop 4
  end function pass_scalar_with_result

  subroutine sub (s1, s2)
    character(len=:), allocatable :: s1, s2
    s1 = scalar ()
    s2 = scalar_with_result ()
    if (len(s1) /= 6) stop 5
    if (len(s2) /= 6) stop 6
  end subroutine sub

  subroutine sub2 (s1, s2)
    character(len=:), allocatable :: s1, s2
    character(len=:), allocatable :: s
    s  = "abc"
    s1 = s
    s  = scalar_with_result ()
    s2 = s
    if (len(s1) /= 3) stop 7
    if (len(s2) /= 6) stop 8
  end subroutine sub2
end program
