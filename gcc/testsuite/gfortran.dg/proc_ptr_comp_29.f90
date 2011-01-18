! { dg-do run }
!
! PR 47240: [F03] segfault with procedure pointer component
!
! Contributed by Martien Hulsen <m.a.hulsen@tue.nl>

  type t
    procedure (fun), pointer, nopass :: p
  end type
  type(t) :: x
  real, dimension(2) :: r
  x%p => fun
  r = evaluate (x%p)
  if (r(1) /= 5 .and. r(2) /= 6) call abort()
contains
  function fun ()
    real, dimension(2) :: fun
    fun = (/ 5, 6 /)
  end function
  function evaluate ( dummy )
    real, dimension(2) :: evaluate
    procedure(fun) :: dummy
    evaluate = dummy ()
  end function
end
