! Tests for MIN and MAX intrinsics with character arguments
!
! { dg-do run }
program test
  character(len=3), parameter :: sp = "gee"
  character(len=6), parameter :: tp = "crunch", wp = "flunch"
  character(len=2), parameter :: up = "az", vp = "da"

  character(len=3) :: s
  character(len=6) :: t, w
  character(len=2) :: u, v
  s = "gee"
  t = "crunch"
  u = "az"
  v = "da"
  w = "flunch"

  if (.not. equal(min("foo", "bar"), "bar")) STOP 1
  if (.not. equal(max("foo", "bar"), "foo")) STOP 2
  if (.not. equal(min("bar", "foo"), "bar")) STOP 3
  if (.not. equal(max("bar", "foo"), "foo")) STOP 4

  if (.not. equal(min("bar", "foo", sp), "bar")) STOP 5
  if (.not. equal(max("bar", "foo", sp), "gee")) STOP 6
  if (.not. equal(min("bar", sp, "foo"), "bar")) STOP 7
  if (.not. equal(max("bar", sp, "foo"), "gee")) STOP 8
  if (.not. equal(min(sp, "bar", "foo"), "bar")) STOP 9
  if (.not. equal(max(sp, "bar", "foo"), "gee")) STOP 10

  if (.not. equal(min("foo", "bar", s), "bar")) STOP 11
  if (.not. equal(max("foo", "bar", s), "gee")) STOP 12
  if (.not. equal(min("foo", s, "bar"), "bar")) STOP 13
  if (.not. equal(max("foo", s, "bar"), "gee")) STOP 14
  if (.not. equal(min(s, "foo", "bar"), "bar")) STOP 15
  if (.not. equal(max(s, "foo", "bar"), "gee")) STOP 16

  if (.not. equal(min("", ""), "")) STOP 17
  if (.not. equal(max("", ""), "")) STOP 18
  if (.not. equal(min("", " "), " ")) STOP 19
  if (.not. equal(max("", " "), " ")) STOP 20

  if (.not. equal(min(u,v,w), "az    ")) STOP 21
  if (.not. equal(max(u,v,w), "flunch")) STOP 22
  if (.not. equal(min(u,vp,w), "az    ")) STOP 23
  if (.not. equal(max(u,vp,w), "flunch")) STOP 24
  if (.not. equal(min(u,v,wp), "az    ")) STOP 25
  if (.not. equal(max(u,v,wp), "flunch")) STOP 26
  if (.not. equal(min(up,v,w), "az    ")) STOP 27
  if (.not. equal(max(up,v,w), "flunch")) STOP 28

  call foo("gee   ","az    ",s,t,u,v)
  call foo("gee   ","az    ",s,t,u,v)
  call foo("gee   ","az    ",s,t,u)
  call foo("gee   ","crunch",s,t)

contains

  subroutine foo(res_max, res_min, a, b, c, d)
    character(len=*) :: res_min, res_max
    character(len=*), optional :: a, b, c, d

    if (.not. equal(min(a,b,c,d), res_min)) STOP 29
    if (.not. equal(max(a,b,c,d), res_max)) STOP 30
  end subroutine foo

  pure function equal(a,b)
    character(len=*), intent(in) :: a, b
    logical :: equal

    equal = (len(a) == len(b)) .and. (a == b)
  end function equal

end program test
