! { dg-do compile }
!
! PR 54435: [4.7/4.8 Regression] ICE with SELECT TYPE on a non-CLASS object
!
! Contributed by xarthisius

subroutine foo(x)
  integer :: x
  select type (x)   ! { dg-error "Selector shall be polymorphic" }
  end select
end


! PR 54443: [4.7/4.8 Regression] Segmentation Fault when Compiling for code using Fortran Polymorphic Entities
!
! Contributed by Mark Beyer <mbeyer@cirrusaircraft.com>

program class_test
  type hashnode
    character(4) :: htype
  end type
  class(hashnode), pointer :: hp

  select type(hp%htype)   ! { dg-error "is not a named variable" }

end program
