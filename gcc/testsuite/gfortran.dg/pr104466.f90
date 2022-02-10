! { dg-do compile }
! { dg-options "-std=legacy -O2 --param max-inline-insns-auto=0 --param max-inline-insns-single=0 -fdump-tree-lim2-details" }

      MODULE mod_param
        integer, parameter :: Ngrids = 1
        integer, dimension(Ngrids) :: N
        END  
      MODULE mod_forces
        TYPE T_FORCES
          real, pointer :: sustr(:,:)
          real, pointer :: svstr(:,:)
          real, pointer :: bustr(:,:)
          real, pointer :: bvstr(:,:)
          real, pointer :: srflx(:,:)
          real, pointer :: stflx(:,:,:)
        END TYPE 
        TYPE (T_FORCES), allocatable :: FORCES(:)
      END  
      MODULE mod_grid
        TYPE T_GRID
          real, pointer :: f(:,:)
          real, pointer :: Hz(:,:,:)
          real, pointer :: z_r(:,:,:)
          real, pointer :: z_w(:,:,:)
        END TYPE 
        TYPE (T_GRID), allocatable :: GRID(:)
      END  
      MODULE mod_scalars
        USE mod_param
      END  
      MODULE mod_mixing
        TYPE T_MIXING
          integer,  pointer :: ksbl(:,:)
          real, pointer :: Akv(:,:,:)
          real, pointer :: Akt(:,:,:,:)
          real, pointer :: alpha(:,:)
          real, pointer :: beta(:,:)
          real, pointer :: bvf(:,:,:)
          real, pointer :: hsbl(:,:)
          real, pointer :: ghats(:,:,:,:)
        END TYPE 
        TYPE (T_MIXING), allocatable :: MIXING(:)
      END  
      MODULE mod_ocean
        TYPE T_OCEAN
          real, pointer :: pden(:,:,:)
          real, pointer :: u(:,:,:,:)
          real, pointer :: v(:,:,:,:)
        END TYPE 
        TYPE (T_OCEAN), allocatable :: OCEAN(:)
      END  
      MODULE lmd_skpp_mod
      PRIVATE
      PUBLIC  lmd_skpp
      CONTAINS
      SUBROUTINE lmd_skpp 
      USE mod_forces
      USE mod_grid
      USE mod_mixing
      USE mod_ocean
      integer tile
      integer UBi, UBj 
      CALL lmd_skpp_tile (ng, tile,                                     LBi, UBi, LBj, UBj,                           &
     IminS, ImaxS, JminS, JmaxS,                   nstp0,                                     &
     GRID(ng) % f,                                 GRID(ng) % Hz,                                &
     GRID(ng) % z_r,                               GRID(ng) % z_w,                               &
     OCEAN(ng) % u,                                OCEAN(ng) % v,                                &
     OCEAN(ng) % pden,                             FORCES(ng) % srflx,                           &
     FORCES(ng) % stflx,                           FORCES(ng) % bustr,                           &
     FORCES(ng) % bvstr,                           FORCES(ng) % sustr,                           &
     FORCES(ng) % svstr,                           MIXING(ng) % alpha,                           &
     MIXING(ng) % beta,                            MIXING(ng) % bvf,                             &
     MIXING(ng) % ghats,                           MIXING(ng) % Akt,                             &
     MIXING(ng) % Akv,                             MIXING(ng) % hsbl,                            MIXING(ng) % ksbl)
      END  
      SUBROUTINE lmd_skpp_tile (ng, tile,                               LBi, UBi, LBj, UBj,                     &
     IminS, ImaxS, JminS, JmaxS,             nstp,                                   f, Hz, z_r, z_w,                        &
     u, v, pden,                             srflx, stflx,                           bustr, bvstr, sustr, svstr,             &
     alpha,                                  beta,                                   bvf,                                    &
     ghats,                                  Akt, Akv, hsbl, ksbl)
      USE mod_scalars
      integer tile
      integer UBi, UBj
      real f(:,:)
      real Hz(:,:,:)
      real z_r(:,:,:)
      real z_w(:,:,:)
      real u(:,:,:,:)
      real v(:,:,:,:)
      real pden(:,:,:)
      real srflx(:,:)
      real stflx(:,:,:)
      real alpha(:,:)
      real beta(:,:)
      real bustr(:,:)
      real bvstr(:,:)
      real sustr(:,:)
      real svstr(:,:)
      real bvf(:,:,:)
      real Akt(:,:,:,:)
      real Akv(:,:,:)
      real hsbl(:,:)
      integer ksbl(:,:)
      real ghats(:,:,:,:)
      DO j=Jstr,Jend
        DO iik=IstrIstr,z_w(i,j,N(ng))
          IF (hsbl0.gt.z_w0) THEN
            ksbl=zbl
          END IF
        END DO
      END DO
      END  
      END 

! { dg-final { scan-tree-dump-not ": dependent" "lim2" } }
! { dg-final { scan-tree-dump "Moving statement\[\n\r\]_\[0-9\]+ = n" "lim2" } }
