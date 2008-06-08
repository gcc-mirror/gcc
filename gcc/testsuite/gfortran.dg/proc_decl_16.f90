! { dg-do compile }
! PR fortran/36459
!
abstract interface
  function dim()
    integer :: dim
  end function dim
end interface
procedure(dim) :: f

interface
  integer function tan()
  end function
end interface
procedure(tan) :: g

print *, f()

print *, tan()

end
