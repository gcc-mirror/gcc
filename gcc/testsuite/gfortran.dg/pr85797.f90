! { dg-do compile }
! { dg-options "-Wall" }
! PR fortran/83515 - ICE: Invalid expression in gfc_element_size 
! PR fortran/85797 - ICE in gfc_element_size, at fortran/target-memory.c:126

subroutine a
  c = transfer (a, b)           ! { dg-warning "Non-RECURSIVE procedure" }
end

recursive subroutine d
  c = transfer (d, b)
end

recursive subroutine e
  k = transfer (transfer (e, e), 1)
end

subroutine f
  use, intrinsic :: iso_c_binding
  integer(c_intptr_t) :: b, c
  c = transfer (transfer (b, a), b)
end

module m
contains
  function f () result (z)      ! { dg-warning "Return value" }
    class(*), pointer :: z
  end function f
  recursive subroutine s (q)
    procedure(f) :: q
    call s (q)
  end subroutine s
end
