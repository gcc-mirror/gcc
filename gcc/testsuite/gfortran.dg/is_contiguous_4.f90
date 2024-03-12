! { dg-do run }
! PR fortran/114001 - IS_CONTIGUOUS and polymorphic dummy

program main
  implicit none
  integer :: i, cnt = 0
  logical :: expect
  integer, target  :: m(10) = [(i,i=1,size(m))]
  integer, pointer :: p(:)
  type t
     integer :: j
  end type t
  type(t),  pointer :: tt(:), tp(:)     ! Type pointer
  class(t), pointer :: ct(:), cp(:)     ! Class pointer

  p => m(1:3)
  expect = is_contiguous (p)
  print *, "is_contiguous (p)=", expect
  if (.not. expect) stop 91
  call sub_star (p, expect)
  p => m(1::3)
  expect = is_contiguous (p)
  print *, "is_contiguous (p)=", expect
  if (expect) stop 92
  call sub_star (p, expect)

  allocate (tt(10))
  tt(:)% j = m
  tp => tt(4:6)
  expect = is_contiguous (tp)
  if (.not. expect) stop 96
  print *, "is_contiguous (tp)=", expect
  call sub_t (tp, expect)
  tp => tt(4::3)
  expect = is_contiguous (tp)
  if (expect) stop 97
  print *, "is_contiguous (tp)=", expect
  call sub_t (tp, expect)

  allocate (ct(10))
  ct(:)% j = m
  cp => ct(7:9)
  expect = is_contiguous (cp)
  print *, "is_contiguous (cp)=", expect
  if (.not. expect) stop 98
  call sub_t (cp, expect)
  cp => ct(4::3)
  expect = is_contiguous (cp)
  print *, "is_contiguous (cp)=", expect
  if (expect) stop 99
  call sub_t (cp, expect)

contains

  subroutine sub_star (x, expect)
    class(*), intent(in) :: x(:)
    logical,  intent(in) :: expect
    cnt = cnt + 10
    if (is_contiguous (x) .neqv. expect) then
       print *, "sub_star(1): is_contiguous (x)=", is_contiguous (x), expect
       stop (cnt + 1)
    end if
    select type (x)
    type is (integer)
       if (is_contiguous (x) .neqv. expect) then
          print *, "sub_star(2): is_contiguous (x)=", is_contiguous (x), expect
          stop (cnt + 2)
       end if
    end select
  end

  subroutine sub_t (x, expect)
    class(t), intent(in) :: x(:)
    logical,  intent(in) :: expect
    cnt = cnt + 10
    if (is_contiguous (x) .neqv. expect) then
       print *, "sub_t: is_contiguous (x)=", is_contiguous (x), expect
       stop (cnt + 3)
    end if
  end
end
