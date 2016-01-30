! { dg-do compile }
!
! PR fortran/59746
! Check that symbols present in common block are properly cleaned up
! upon error.
!
! Contributed by Bud Davis  <jmdavis@link.com>

      CALL RCCFL (NVE,IR,NU3,VE (1,1,1,I))
      COMMON /CCFILE/ INTG,NT1,NT2,NT3,NVM,NVE,NFRLE,NRESF,NRESL !{ dg-error "Unexpected COMMON" }
      COMMON /CCFILE/ INTG,NT1,NT2,NT3,NVM,NVE,NFRLE,NRESF,NRESL !{ dg-error "Unexpected COMMON" }
!  the PR only contained the two above.
!  success is no segfaults or infinite loops.
!  let's check some combinations
     CALL ABC (INTG)
     COMMON /CCFILE/ INTG,NT1,NT2,NT3,NVM,NVE,NFRLE,NRESF,NRESL !{ dg-error "Unexpected COMMON" }
     COMMON /CCFILE/ INTG,NT1,NT2,NT3,NVM,NVE,NFRLE,NRESF,NRESL !{ dg-error "Unexpected COMMON" }
     CALL DEF (NT1)
     COMMON /CCFILE/ INTG,NT1,NT2,NT3,NVM,NVE,NFRLE,NRESF,NRESL !{ dg-error "Unexpected COMMON" }
     COMMON /CCFILE/ INTG,NT1,NT2,NT3,NVM,NVE,NFRLE,NRESF,NRESL !{ dg-error "Unexpected COMMON" }
     CALL GHI (NRESL)
     COMMON /CCFILE/ INTG,NT1,NT2,NT3,NVM,NVE,NFRLE,NRESF,NRESL !{ dg-error "Unexpected COMMON" }
     COMMON /CCFILE/ INTG,NT1,NT2,NT3,NVM,NVE,NFRLE,NRESF,NRESL !{ dg-error "Unexpected COMMON" }
     END
