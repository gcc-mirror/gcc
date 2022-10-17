module main
  implicit none
contains
  function f1 (x, y, z)
    real (kind = 8) :: f1
    integer, intent(in) :: x
    integer (kind = 8), intent(in) :: y
    real :: z

    f1 = 0.0
  end function

  function f2 (x, y, z)
    real (kind = 8) :: f2
    integer, intent(in) :: x
    integer (kind = 8), intent(in) :: y
    real :: z

    f2 = 0.0
  end function

  function f3 (x, y, z)
    real (kind = 8) :: f3
    integer, intent(in) :: x
    integer (kind = 8), intent(in) :: y
    real :: z
    !$omp declare variant (f1) match (user={condition(0)},construct={parallel})
    f3 = 0.0
  end function

  function f4 (x, y, z)
    real (kind = 8) :: f4
    integer, intent(in) :: x
    integer (kind = 8), intent(in) :: y
    real :: z
    !$omp declare variant (f1) match (construct={parallel},user={condition(score(1):1)})
    f4 = 0.0
  end function

  function f5 (x, y, z)
    real (kind = 8) :: f5
    integer, intent(in) :: x
    integer (kind = 8), intent(in) :: y
    real :: z
    f5 = 0.0
  end function

  function f6 (x, y, z)
    real (kind = 8) :: f6
    integer, intent(in) :: x
    integer (kind = 8), intent(in) :: y
    real :: z
    !$omp declare variant (f5) match (user={condition(0)})  ! { dg-error "'f5' used as a variant with incompatible 'construct' selector sets" }
    f6 = 0.0
  end function

  function f7 (x, y, z)
    real (kind = 8) :: f7
    integer, intent(in) :: x
    integer (kind = 8), intent(in) :: y
    real :: z
    !$omp declare variant (f5) match (construct={parallel},user={condition(score(1):1)})
    f7 = 0.0
  end function

  function f8 (x, y, z)
    real (kind = 8) :: f8
    integer, intent(in) :: x
    integer (kind = 8), intent(in) :: y
    real :: z
    f8 = 0.0
  end function

  function f9 (x, y, z)
    real (kind = 8) :: f9
    integer, intent(in) :: x
    integer (kind = 8), intent(in) :: y
    real :: z
    !$omp declare variant (f8) match (user={condition(0)},construct={do})  ! { dg-error "'f8' used as a variant with incompatible 'construct' selector sets" }
    f9 = 0.0
  end function

  function f10 (x, y, z)
    real (kind = 8) :: f10
    integer, intent(in) :: x
    integer (kind = 8), intent(in) :: y
    real :: z
    !$omp declare variant (f8) match (user={condition(1)})
    f10 = 0.0
  end function

  function f11 (x, y, z)
    real (kind = 8) :: f11
    integer, intent(in) :: x
    integer (kind = 8), intent(in) :: y
    real :: z
    f11 = 0.0
  end function

  function f12 (x, y, z)
    real (kind = 8) :: f12
    integer, intent(in) :: x
    integer (kind = 8), intent(in) :: y
    real :: z
    !$omp declare variant (f11) match (construct={target,teams,parallel,do})  ! { dg-error "'f11' used as a variant with incompatible 'construct' selector sets" }
    f12 = 0.0
  end function

  function f13 (x, y, z)
    real (kind = 8) :: f13
    integer, intent(in) :: x
    integer (kind = 8), intent(in) :: y
    real :: z
    !$omp declare variant (f11) match (user={condition(score(1):1)},construct={target,teams,parallel,do})  ! { dg-error "'f11' used as a variant with incompatible 'construct' selector sets" }
    f13 = 0.0
  end function

  function f14 (x, y, z)
    real (kind = 8) :: f14
    integer, intent(in) :: x
    integer (kind = 8), intent(in) :: y
    real :: z
    !$omp declare variant (f11) match (implementation={vendor(gnu)},construct={target,teams,parallel})  ! { dg-error "'f11' used as a variant with incompatible 'construct' selector sets" }
    f14 = 0.0
  end function

  function f15 (x, y, z)
    real (kind = 8) :: f15
    integer, intent(in) :: x
    integer (kind = 8), intent(in) :: y
    real :: z
    !$omp declare variant (f11) match (device={kind(any)},construct={teams,parallel})
    f15 = 0.0
  end function

  function f16 (x, y, z)
    real (kind = 8) :: f16
    integer, intent(in) :: x
    integer (kind = 8), intent(in) :: y
    real :: z
    f16 = 0.0
  end function

  function f17 (x, y, z)
    real (kind = 8) :: f17
    integer, intent(in) :: x
    integer (kind = 8), intent(in) :: y
    real :: z
    !$omp declare variant (f16) match (construct={teams,parallel})  ! { dg-error "'f16' used as a variant with incompatible 'construct' selector sets" }
    f17 = 0.0
  end function

  function f18 (x, y, z)
    real (kind = 8) :: f18
    integer, intent(in) :: x
    integer (kind = 8), intent(in) :: y
    real :: z
    !$omp declare variant (f16) match(construct={teams,parallel,do})
    f18 = 0.0
  end function

  function f19 (x, y, z)
    real (kind = 8) :: f19
    integer, intent(in) :: x
    integer (kind = 8), intent(in) :: y
    real :: z
    f19 = 0.0
  end function

  function f20 (x, y, z)
    real (kind = 8) :: f20
    integer, intent(in) :: x
    integer (kind = 8), intent(in) :: y
    real :: z
    !$omp declare variant (f19) match (construct={parallel})  ! { dg-error "'f19' used as a variant with incompatible 'construct' selector sets" }
    f20 = 0.0
  end function

  function f21 (x, y, z)
    real (kind = 8) :: f21
    integer, intent(in) :: x
    integer (kind = 8), intent(in) :: y
    real :: z
    !$omp declare variant (f19) match (construct={do},implementation={vendor(gnu,llvm)})
    f21 = 0.0
  end function

end module
