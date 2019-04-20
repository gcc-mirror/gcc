! { dg-do compile }
! PR fortran/83515 - ICE: Invalid expression in gfc_element_size 
! PR fortran/85797 - ICE in gfc_element_size, at fortran/target-memory.c:126
! PR fortran/89904 - ICE in gfortran starting with r270045

recursive subroutine a
  c = transfer (a, b)           ! { dg-error "'SOURCE' argument of 'TRANSFER'" }
end

recursive subroutine d
  c = transfer (b, d)           ! { dg-error "'MOLD' argument of 'TRANSFER'" }
end

subroutine f
  use, intrinsic :: iso_c_binding
  integer(c_intptr_t) :: b, c
  procedure(), pointer :: a
  c = transfer (a, b)
  c = transfer (transfer (b, a), b)
end

module m
contains
  function f () result (z)
    class(*), pointer :: z
  end function f
  recursive subroutine s (q)
    procedure(f) :: q
    call s (q)
  end subroutine s
end
