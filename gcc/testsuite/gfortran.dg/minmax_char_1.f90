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

  if (.not. equal(min("foo", "bar"), "bar")) call abort
  if (.not. equal(max("foo", "bar"), "foo")) call abort
  if (.not. equal(min("bar", "foo"), "bar")) call abort
  if (.not. equal(max("bar", "foo"), "foo")) call abort

  if (.not. equal(min("bar", "foo", sp), "bar")) call abort
  if (.not. equal(max("bar", "foo", sp), "gee")) call abort
  if (.not. equal(min("bar", sp, "foo"), "bar")) call abort
  if (.not. equal(max("bar", sp, "foo"), "gee")) call abort
  if (.not. equal(min(sp, "bar", "foo"), "bar")) call abort
  if (.not. equal(max(sp, "bar", "foo"), "gee")) call abort

  if (.not. equal(min("foo", "bar", s), "bar")) call abort
  if (.not. equal(max("foo", "bar", s), "gee")) call abort
  if (.not. equal(min("foo", s, "bar"), "bar")) call abort
  if (.not. equal(max("foo", s, "bar"), "gee")) call abort
  if (.not. equal(min(s, "foo", "bar"), "bar")) call abort
  if (.not. equal(max(s, "foo", "bar"), "gee")) call abort

  if (.not. equal(min("", ""), "")) call abort
  if (.not. equal(max("", ""), "")) call abort
  if (.not. equal(min("", " "), " ")) call abort
  if (.not. equal(max("", " "), " ")) call abort

  if (.not. equal(min(u,v,w), "az    ")) call abort
  if (.not. equal(max(u,v,w), "flunch")) call abort
  if (.not. equal(min(u,vp,w), "az    ")) call abort
  if (.not. equal(max(u,vp,w), "flunch")) call abort
  if (.not. equal(min(u,v,wp), "az    ")) call abort
  if (.not. equal(max(u,v,wp), "flunch")) call abort
  if (.not. equal(min(up,v,w), "az    ")) call abort
  if (.not. equal(max(up,v,w), "flunch")) call abort

  call foo("gee   ","az    ",s,t,u,v)
  call foo("gee   ","az    ",s,t,u,v)
  call foo("gee   ","az    ",s,t,u)
  call foo("gee   ","crunch",s,t)

contains

  subroutine foo(res_max, res_min, a, b, c, d)
    character(len=*) :: res_min, res_max
    character(len=*), optional :: a, b, c, d

    if (.not. equal(min(a,b,c,d), res_min)) call abort
    if (.not. equal(max(a,b,c,d), res_max)) call abort
  end subroutine foo

  pure function equal(a,b)
    character(len=*), intent(in) :: a, b
    logical :: equal

    equal = (len(a) == len(b)) .and. (a == b)
  end function equal

end program test
