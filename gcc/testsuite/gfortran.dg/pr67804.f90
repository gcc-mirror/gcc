! { dg-do compile }
! PR fortran/67804 - ICE on bad type in structure constructor in DATA statement
! Contributed by G.Steinmetz

program p
  type t
     character :: c
  end type
  type u
     character, pointer :: c
  end type
  type(t) :: x0, x1, x2, x3, x4, x5, x6, x7, x8, x9
  type(u) :: y6
  data x0 /t('a')/     ! OK
  data x1 /t(1)/       ! { dg-error "Cannot convert" }
  data x2 /t(1.)/      ! { dg-error "Cannot convert" }
  data x3 /t(1d1)/     ! { dg-error "Cannot convert" }
  data x4 /t((0.,1.))/ ! { dg-error "Cannot convert" }
  data x5 /t(.true.)/  ! { dg-error "Cannot convert" }
  data x6 /t(null())/  ! { dg-error "neither a POINTER nor ALLOCATABLE" }
  data x7 /t(['1'])/   ! { dg-error "The rank of the element" }
  data x8 /t([1])/     ! { dg-error "Cannot convert" }
  data x9 /t(z'0')/    ! { dg-error "Cannot convert" }
  data y6 /u(null())/  ! OK
end
