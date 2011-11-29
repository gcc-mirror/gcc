! { dg-do compile }
!
! PR 48699: [OOP] MOVE_ALLOC inside SELECT TYPE
!
! Contributed by Salvatore Filippone <sfilippone@uniroma2.it>
!
! Note that per Fortran 2008, 8.1.9.2, "within the block following
! a TYPE IS type guard statement, the associating entity (16.5.5) is not polymorphic"
!

program testmv2

  type bar
    integer, allocatable  :: ia(:), ja(:)
  end type bar

  class(bar), allocatable :: sm,sm2

  allocate(sm2)

  select type(sm2) 
  type is (bar)
    call move_alloc(sm2,sm) ! { dg-error "must be either both polymorphic or both nonpolymorphic" }
  end select

end program testmv2
