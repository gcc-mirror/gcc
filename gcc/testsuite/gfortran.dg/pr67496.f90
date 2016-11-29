! PR fortran/67496
! { dg-do compile }

  type :: a
  end type a
  type :: b
    type (a) :: j(1)
  end type b
  type(a) :: x
  type(b) :: y
  y = b((/x/))
end
