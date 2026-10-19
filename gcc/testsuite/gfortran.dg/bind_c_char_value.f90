! { dg-do run }
! { dg-additional-sources bind_c_char_value_aux.c }
!
! PR fortran/127881 - CHARACTER(c_char),VALUE dummy of BIND(C) interface
!
! Contributed by Christopher Albert <albert@tugraz.at>

module m
  use, intrinsic :: iso_c_binding, only: c_char, c_int
  implicit none
  interface
     function echo_char(a) bind(C, name="echo_char") result(r)
       import :: c_char, c_int
       character(kind=c_char), value, intent(in) :: a
       integer(c_int) :: r
     end function echo_char
  end interface
contains
  integer function via_dummy(c)
    character(kind=c_char), intent(in) :: c
    character(kind=c_char,len=2) :: cc
    integer :: res, tmp, n = 2
    via_dummy = echo_char(c)
    res = via_dummy
    ! test non-variable argument a and len(a) /= 1
    tmp = echo_char ((c))
    if (tmp /= res) error stop 11
    cc = c // c
    tmp = echo_char (cc)
    if (tmp /= res) error stop 12
    tmp = echo_char ((cc))
    if (tmp /= res) error stop 13
    tmp = echo_char (c // c)
    if (tmp /= res) error stop 14
    tmp = echo_char(repeat (c,n))
    if (tmp /= res) error stop 15
  end function via_dummy
end module m

program pr
  use m
  implicit none
  integer :: i, nbad_local, nbad_dummy, nbad_achar
  character(kind=c_char) :: c
  nbad_local = 0; nbad_dummy = 0; nbad_achar = 0
  do i = 0, 127
     c = achar(i)
     if (echo_char(c) /= i) nbad_local = nbad_local + 1
     if (via_dummy(c) /= i) nbad_dummy = nbad_dummy + 1
     if (echo_char(achar(i)) /= i) nbad_achar = nbad_achar + 1
  end do
  if (nbad_local + nbad_dummy + nbad_achar /= 0) then
     print '(3(a,i0))', 'local variable: ', nbad_local, &
          '  dummy argument: ', nbad_dummy, '  achar(): ', nbad_achar
     stop 1
  end if
end program pr
