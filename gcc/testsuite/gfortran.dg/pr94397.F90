! { dg-do run }
!

module m
  implicit none
contains
  function is_real8(a)
    class(*) :: a
    logical :: is_real8
    is_real8 = .false.
    select type(a)
      type is(real(kind(1.0_8)))
        is_real8 = .true. 
    end select
  end function is_real8
end module m

program test
  use m

  if (is_real8(1.0_4)) stop 1
  if (.not. is_real8(1.0_8)) stop 2
#ifdef __GFC_REAL_16__
  if (is_real8(1.0_16)) stop 3
#endif
end program
