! { dg-do run }
! PR95542 - ICE in gfc_get_symbol_decl, at fortran/trans-decl.cc:1851
!
function f()
  character(:), allocatable :: f
  f = 'xyz'
  call s
contains
  subroutine s
    if (f /= 'xyz') stop 1
  end subroutine s
end function f

program pr95542
  interface
    function f()
      character(:), allocatable :: f
    end function f
  end interface
  if (f() /= 'xyz') stop 2
end program pr95542
