! PR fortran/124450
! { dg-do compile }

  type ta
    integer(kind=4) :: a(1)
    integer(kind=4) :: b(1)
  end type ta
  type tb
    type(ta) :: c(1) = ta(1, 2)
  end type tb
  type(tb) :: e = tb(ta(3, 4))

  print *, e
end
