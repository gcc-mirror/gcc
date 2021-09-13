! { dg-do run }
! { dg-additional-options "-fdump-tree-original" }
! PR fortran/100950 - ICE in output_constructor_regular_field, at varasm.c:5514

program p
  character(8), parameter :: u = "123"
  character(8)            :: x = "", s
  character(2)            :: w(2) = [character(len(x(3:4))) :: 'a','b' ]
  character(*), parameter :: y(*) = [character(len(u(3:4))) :: 'a','b' ]
  character(*), parameter :: z(*) = [character(len(x(3:4))) :: 'a','b' ]
  character(*), parameter :: t(*) = [character(len(x( :2))) :: 'a','b' ]
  character(*), parameter :: v(*) = [character(len(x(7: ))) :: 'a','b' ]
  type t_
     character(len=5)              :: s
     character(len=8)              :: t(4)
     character(len=8), pointer     :: u(:)
     character(len=:), allocatable :: str
     character(len=:), allocatable :: str2(:)
  end type t_
  type(t_)                :: q, r(1)
  integer,      parameter :: lq = len (q%s(3:4)), lr = len (r%s(3:4))
  integer,      parameter :: l1 = len (q   %t(1)(3:4))
  integer,      parameter :: l2 = len (q   %t(:)(3:4))
  integer,      parameter :: l3 = len (q   %str (3:4))
  integer,      parameter :: l4 = len (r(:)%t(1)(3:4))
  integer,      parameter :: l5 = len (r(1)%t(:)(3:4))
  integer,      parameter :: l6 = len (r(1)%str (3:4))
  integer,      parameter :: l7 = len (r(1)%str2(1)(3:4))
  integer,      parameter :: l8 = len (r(1)%str2(:)(3:4))

  if (len (y) /= 2) stop 1
  if (len (z) /= 2) stop 2
  if (any (w /= y)) stop 3
  if (len ([character(len(u(3:4))) :: 'a','b' ]) /= 2)  stop 4
  if (len ([character(len(x(3:4))) :: 'a','b' ]) /= 2)  stop 5
  if (any ([character(len(x(3:4))) :: 'a','b' ]  /= y)) stop 6
  write(s,*) [character(len(x(3:4))) :: 'a','b' ]
  if (s /= " a b    ") stop 7
  if (len (t) /= 2) stop 8
  if (len (v) /= 2) stop 9
  if (lq /= 2 .or. lr /= 2) stop 10
  if (l1 /= 2 .or. l2 /= 2 .or. l4 /= 2 .or. l5 /= 2) stop 11
  if (l3 /= 2 .or. l6 /= 2 .or. l7 /= 2 .or. l8 /= 2) stop 12

  block
    integer, parameter :: l9 = len (r(1)%u(:)(3:4))
    if (l9 /= 2) stop 13
  end block

  call sub (42, "abcde")
contains
  subroutine sub (m, c)
    integer,          intent(in) :: m
    character(len=*), intent(in) :: c
    character(len=m)    :: p, o(3)
    integer, parameter  :: l10 = len (p(6:7))
    integer, parameter  :: l11 = len (o(:)(6:7))
    integer, parameter  :: l12 = len (c(2:3))
    if (l10 /= 2 .or. l11 /= 2 .or. l12 /= 2) stop 14
  end subroutine sub
end

! { dg-final { scan-tree-dump-times "_gfortran_stop_numeric" 2 "original" } }
! { dg-final { scan-tree-dump "_gfortran_stop_numeric \\(3, 0\\);" "original" } }
! { dg-final { scan-tree-dump "_gfortran_stop_numeric \\(7, 0\\);" "original" } }
