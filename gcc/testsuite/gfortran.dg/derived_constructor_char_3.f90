! { dg-do compile }
!
! PR fortran/51966
!
! Contributed by Peter Wind
!

  type :: Deriv
    character(len=10) :: name
  end type
  character(len=8), dimension(2), parameter :: &
       DEF_ECOSYSTEMS = (/ "Gridxxxx", "StringYY" /)

  type(Deriv), save :: DepEcoSystem = Deriv(DEF_ECOSYSTEMS(1))

  if (DepEcoSystem%name /= "Gridxxxx" &
      .or. DepEcoSystem%name(9:9) /= ' ' &
      .or. DepEcoSystem%name(10:10) /= ' ') call abort()
  DepEcoSystem%name = 'ABCDEFGHIJ'
  call Init_EcoSystems()
  if (DepEcoSystem%name /= "StringYY" &
      .or. DepEcoSystem%name(9:9) /= ' ' &
      .or. DepEcoSystem%name(10:10) /= ' ') call abort()

contains
  subroutine Init_EcoSystems()
    integer :: i =2
    DepEcoSystem = Deriv(DEF_ECOSYSTEMS(i))
  end subroutine Init_EcoSystems
end
