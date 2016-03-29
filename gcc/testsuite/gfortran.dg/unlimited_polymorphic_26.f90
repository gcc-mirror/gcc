! { dg-do run }
!
! Test contributed by Valery Weber  <valeryweber@hotmail.com>

module mod

  TYPE, PUBLIC :: dict_entry_type
     CLASS( * ), ALLOCATABLE :: key
     CLASS( * ), ALLOCATABLE :: val
  END TYPE dict_entry_type


contains

  SUBROUTINE dict_put ( this, key, val )
    CLASS(dict_entry_type), INTENT(INOUT)     :: this
    CLASS(*), INTENT(IN)                     :: key, val
    INTEGER                                  :: istat
    ALLOCATE( this%key, SOURCE=key, STAT=istat )
    ALLOCATE( this%val, SOURCE=val, STAT=istat )
  end SUBROUTINE dict_put
end module mod

program test
  use mod
  type(dict_entry_type) :: t
  call dict_put(t, "foo", 42)

  if (.NOT. allocated(t%key)) call abort()
  select type (x => t%key)
    type is (CHARACTER(*))
      if (x /= "foo") call abort()
    class default
      call abort()
  end select
  deallocate(t%key)

  if (.NOT. allocated(t%val)) call abort()
  select type (x => t%val)
    type is (INTEGER)
      if (x /= 42) call abort()
    class default
      call abort()
  end select
  deallocate(t%val)
end

