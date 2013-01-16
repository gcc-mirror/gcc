! { dg-do compile }
!
! PR 55983: [4.7/4.8 Regression] ICE in find_typebound_proc_uop, at fortran/class.c:2711
!
! Contributed by Sylwester Arabas <slayoo@staszic.waw.pl>

  type :: mpdata_t
    class(bcd_t), pointer :: bcx, bcy   ! { dg-error "is a type that has not been declared" }
  end type
  type(mpdata_t) :: this
  call this%bcx%fill_halos()            ! { dg-error "is being used before it is defined" }
end
