! { dg-do run }
! See attachment to PR82173 comment 10 - reproducers/bug22-import-pdt-ice.f90
module t_m
  type :: t(n)
    integer, len :: n = 1
    real :: v(n)
  end type
end module t_m

module t2_m
  use t_m
  interface
    subroutine handler (a)
      import :: t
      type(t(*)), intent(in) :: a
    end subroutine
  end interface
end module t2_m

subroutine handler (a)
  use t_m
  type(t(*)), intent(in) :: a
  if (any (int (a%v) /= [1,2,3])) stop 1
end subroutine

  use t2_m
  type(t(3)) :: x
  x%v = [1.,2.,3.]
  call handler (x)
end
