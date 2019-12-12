! { dg-do compile }
!
! PR 89601: [8/9 Regression] [PDT] ICE: Segmentation fault (in resolve_component)
!
! Contributed by Arseny Solokha <asolokha@gmx.com>

program vw
  interface
     real function ul (ki)
       real :: ki
     end function ul
  end interface
  type :: q8 ()  ! { dg-error "A type parameter list is required" }
     procedure (ul), pointer, nopass :: pj
  end type q8
  type (q8) :: ki
end program vw
