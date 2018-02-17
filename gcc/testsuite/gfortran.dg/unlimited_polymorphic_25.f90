! { dg-do run }
!
! Test contributed by Valery Weber  <valeryweber@hotmail.com>

module mod

  TYPE, PUBLIC :: base_type
  END TYPE base_type

  TYPE, PUBLIC :: dict_entry_type
     CLASS( * ), ALLOCATABLE :: key
     CLASS( * ), ALLOCATABLE :: val
  END TYPE dict_entry_type


contains

  SUBROUTINE dict_put ( this, key, val )
    CLASS(dict_entry_type), INTENT(INOUT)     :: this
    CLASS(base_type), INTENT(IN)             :: key, val
    INTEGER                                  :: istat
    ALLOCATE( this%key, SOURCE=key, STAT=istat )
  end SUBROUTINE dict_put
end module mod

program test
  use mod
  type(dict_entry_type) :: t
  type(base_type) :: a, b
  call dict_put(t, a, b)

  if (.NOT. allocated(t%key)) STOP 1
  select type (x => t%key)
    type is (base_type)
    class default
      STOP 2
  end select
  deallocate(t%key)
end

