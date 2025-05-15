! { dg-do run }
! { dg-additional-options "-O2 -Wreturn-type" }
!
! PR fortran/85750 - default-initialization and functions returning derived type

module bar
  implicit none
  type ilist
    integer          :: count = 42
    integer, pointer :: ptr(:) => null()
  end type ilist

  type jlist
    real, allocatable :: a(:)
    integer           :: count = 23
  end type jlist

contains

  function make_list(i)
    integer,     intent(in)   :: i
    type(ilist), dimension(2) :: make_list
    make_list(i)%count = i
  end function make_list

  function make_list_res(i) result(list)
    integer,     intent(in)   :: i
    type(ilist), dimension(2) :: list
    list(i)%count = i
  end function make_list_res

  function make_jlist(i)
    integer,     intent(in)   :: i
    type(jlist), dimension(2) :: make_jlist
    make_jlist(i)%count = i
  end function make_jlist

  function make_jlist_res(i) result(list)
    integer,     intent(in)   :: i
    type(jlist), dimension(2) :: list
    list(i)%count = i
  end function make_jlist_res

  function empty_ilist()
    type(ilist), dimension(2) :: empty_ilist
  end function

  function empty_jlist()
    type(jlist), dimension(2) :: empty_jlist
  end function

  function empty_ilist_res() result (res)
    type(ilist), dimension(2) :: res
  end function

  function empty_jlist_res() result (res)
    type(jlist), dimension(2) :: res
  end function

end module bar

program foo
  use bar
  implicit none
  type(ilist)              :: mylist(2) = ilist(count=-2)
  type(jlist), allocatable :: yourlist(:)

  mylist = ilist(count=-1)
  if (any (mylist%count /= [-1,-1])) stop 1
  mylist = empty_ilist()
  if (any (mylist%count /= [42,42])) stop 2
  mylist = ilist(count=-1)
  mylist = empty_ilist_res()
  if (any (mylist%count /= [42,42])) stop 3

  allocate(yourlist(1:2))
  if (any (yourlist%count /= [23,23])) stop 4
  yourlist = jlist(count=-1)
  if (any (yourlist%count /= [-1,-1])) stop 5
  yourlist = empty_jlist()
  if (any (yourlist%count /= [23,23])) stop 6
  yourlist = jlist(count=-1)
  yourlist = empty_jlist_res()
  if (any (yourlist%count /= [23,23])) stop 7

  mylist = make_list(1)
  if (any (mylist%count /= [1,42])) stop 11
  mylist = make_list(2)
  if (any (mylist%count /= [42,2])) stop 12
  mylist = (make_list(1))
  if (any (mylist%count /= [1,42])) stop 13
  mylist = [make_list(2)]
  if (any (mylist%count /= [42,2])) stop 14

  mylist = make_list_res(1)
  if (any (mylist%count /= [1,42])) stop 21
  mylist = make_list_res(2)
  if (any (mylist%count /= [42,2])) stop 22
  mylist = (make_list_res(1))
  if (any (mylist%count /= [1,42])) stop 23
  mylist = [make_list_res(2)]
  if (any (mylist%count /= [42,2])) stop 24

  yourlist = make_jlist(1)
  if (any (yourlist%count /= [1,23])) stop 31
  yourlist = make_jlist(2)
  if (any (yourlist%count /= [23,2])) stop 32
  yourlist = (make_jlist(1))
  if (any (yourlist%count /= [1,23])) stop 33
  yourlist = [make_jlist(2)]
  if (any (yourlist%count /= [23,2])) stop 34

  yourlist = make_jlist_res(1)
  if (any (yourlist%count /= [1,23])) stop 41
  yourlist = make_jlist_res(2)
  if (any (yourlist%count /= [23,2])) stop 42
  yourlist = (make_jlist_res(1))
  if (any (yourlist%count /= [1,23])) stop 43
  yourlist = [make_jlist_res(2)]
  if (any (yourlist%count /= [23,2])) stop 44

  deallocate (yourlist)
end program foo
