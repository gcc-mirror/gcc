! { dg-do compile }
! { dg-options "-fcheck=all" }
!
! PR fortran/57542
!
! Contributed by Salvatore Filippone
!
module type_mod
  type inner
  end type inner

  type outer 
    class(inner), allocatable :: item
  end type outer

  type container 
    class(outer), allocatable :: item
  end type container

  type maintype
    type(container), allocatable :: v(:)
  end type maintype

end module type_mod

subroutine testfinal(var)
  use type_mod
  type(maintype), intent(inout) :: var
  ! A real code would obviously check
  ! this is really allocated
  deallocate(var%v(1)%item%item)
end subroutine testfinal
