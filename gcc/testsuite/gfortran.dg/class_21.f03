! { dg-do compile }
!
! PR 43990: [OOP] ICE in output_constructor_regular_field, at varasm.c:4995
!
! Reported by Hans-Werner Boschmann <boschmann@tp1.physik.uni-siegen.de>

module m

  type :: t
     logical :: l = .true.
     class(t),pointer :: cp => null()
  end type

  type(t),save :: default_t

end module

! { dg-final { cleanup-modules "m" } }
