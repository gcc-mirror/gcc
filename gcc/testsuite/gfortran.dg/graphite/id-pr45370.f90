!
  type :: t
    real :: r
    integer :: i
    character(3) :: chr
  end type t

  type :: t2
    real :: r(2, 2)
    integer :: i
    character(3) :: chr
  end type t2

  type :: s
    type(t), pointer :: t(:)
  end type s

  integer, parameter :: sh(2) = (/2,2/)
  real, parameter :: a1(2,2) = reshape ((/1.0,2.0,3.0,4.0/),sh)
  real, parameter :: a2(2,2) = reshape ((/5.0,6.0,7.0,8.0/),sh)

  type(t), target :: tar1(2) = (/t(1.0, 2, "abc"), t(3.0, 4, "efg")/)
  character(4), target :: tar2(2) = (/"abcd","efgh"/)
  type(s), target :: tar3
  character(2), target :: tar4(2) = (/"ab","cd"/)
  type(t2), target :: tar5(2) = (/t2(a1, 2, "abc"), t2(a2, 4, "efg")/)

  integer, pointer :: ptr(:)
  character(2), pointer :: ptr2(:)
  real, pointer :: ptr3(:)

!_______________component subreference___________
  ptr => tar1%i
  ptr = ptr + 1              ! check the scalarizer is OK

  if (any (ptr .ne. (/3, 5/))) call abort ()
  if (any ((/ptr(1), ptr(2)/) .ne. (/3, 5/))) call abort ()
  if (any (tar1%i .ne. (/3, 5/))) call abort ()

! Make sure that the other components are not touched.
  if (any (tar1%r .ne. (/1.0, 3.0/))) call abort ()
  if (any (tar1%chr .ne. (/"abc", "efg"/))) call abort ()

! Check that the pointer is passed correctly as an actual argument.
  call foo (ptr)
  if (any (tar1%i .ne. (/2, 4/))) call abort ()

! And that dummy pointers are OK too.
  call bar (ptr)
  if (any (tar1%i .ne. (/101, 103/))) call abort ()

!_______________substring subreference___________
  ptr2 => tar2(:)(2:3)
  ptr2 = ptr2(:)(2:2)//"z"   ! again, check the scalarizer

  if (any (ptr2 .ne. (/"cz", "gz"/))) call abort ()
  if (any ((/ptr2(1), ptr2(2)/) .ne. (/"cz", "gz"/))) call abort ()
  if (any (tar2 .ne. (/"aczd", "egzh"/))) call abort ()

!_______________substring component subreference___________
  ptr2 => tar1(:)%chr(1:2)
  ptr2 = ptr2(:)(2:2)//"q"   ! yet again, check the scalarizer
  if (any (ptr2 .ne. (/"bq","fq"/))) call abort ()
  if (any (tar1%chr .ne. (/"bqc","fqg"/))) call abort ()

!_______________trailing array element subreference___________
  ptr3 => tar5%r(1,2)
  ptr3 = (/99.0, 999.0/)
  if (any (tar5(1)%r .ne. reshape ((/1.0,2.0,99.0,4.0/), sh))) call abort ()
  if (any (tar5(2)%r .ne. reshape ((/5.0,6.0,999.0,8.0/), sh))) call abort ()

!_______________forall assignment___________
  ptr2 => tar2(:)(1:2)
  forall (i = 1:2) ptr2(i)(1:1) = "z"
  if (any (tar2 .ne. (/"zczd", "zgzh"/))) call abort ()

!_______________something more complicated___________
  tar3%t => tar1
  ptr3 => tar3%t%r
  ptr3 = cos (ptr3)
  if (any (abs(ptr3 - (/cos(1.0_4), cos(3.0_4)/)) >= epsilon(1.0_4))) call abort ()

  ptr2 => tar3%t(:)%chr(2:3)
  ptr2 = " x"
  if (any (tar1%chr .ne. (/"b x", "f x"/))) call abort ()

!_______________check non-subref works still___________
  ptr2 => tar4
  if (any (ptr2 .ne. (/"ab","cd"/))) call abort ()

contains
  subroutine foo (arg)
    integer :: arg(:)
    arg = arg - 1
  end subroutine
  subroutine bar (arg)
    integer, pointer :: arg(:)
    arg = arg + 99
  end subroutine
end
