    MODULE kinds
      INTEGER, PARAMETER :: DP = selected_real_kind(14,200)
      PRIVATE
      PUBLIC :: DP
    END MODULE kinds

MODULE constants
  USE kinds, ONLY : DP
  IMPLICIT NONE
  SAVE
  REAL(DP), PARAMETER :: pi = 3.14159265358979323846_DP
  REAL(DP), PARAMETER :: tpi= 2.0_DP * pi
  REAL(DP), PARAMETER :: fpi= 4.0_DP * pi
  REAL(DP), PARAMETER :: sqrtpi = 1.77245385090551602729_DP 
  REAL(DP), PARAMETER :: sqrtpm1= 1.0_DP / sqrtpi
  REAL(DP), PARAMETER :: sqrt2  = 1.41421356237309504880_DP
  REAL(DP), PARAMETER :: H_PLANCK_SI      = 6.6260693D-34    ! J s
  REAL(DP), PARAMETER :: K_BOLTZMANN_SI   = 1.3806505D-23    ! J K^-1 
  REAL(DP), PARAMETER :: ELECTRON_SI      = 1.60217653D-19   ! C
  REAL(DP), PARAMETER :: ELECTRONVOLT_SI  = 1.60217653D-19   ! J  
  REAL(DP), PARAMETER :: ELECTRONMASS_SI  = 9.1093826D-31    ! Kg
  REAL(DP), PARAMETER :: HARTREE_SI       = 4.35974417D-18   ! J
  REAL(DP), PARAMETER :: RYDBERG_SI       = HARTREE_SI/2.0_DP! J
  REAL(DP), PARAMETER :: BOHR_RADIUS_SI   = 0.5291772108D-10 ! m
  REAL(DP), PARAMETER :: AMU_SI           = 1.66053886D-27   ! Kg
  REAL(DP), PARAMETER :: K_BOLTZMANN_AU   = K_BOLTZMANN_SI / HARTREE_SI
  REAL(DP), PARAMETER :: K_BOLTZMANN_RY   = K_BOLTZMANN_SI / RYDBERG_SI
  REAL(DP), PARAMETER :: AUTOEV           = HARTREE_SI / ELECTRONVOLT_SI
  REAL(DP), PARAMETER :: RYTOEV           = AUTOEV / 2.0_DP
  REAL(DP), PARAMETER :: AMU_AU           = AMU_SI / ELECTRONMASS_SI
  REAL(DP), PARAMETER :: AMU_RY           = AMU_AU / 2.0_DP
  REAL(DP), PARAMETER :: AU_SEC           = H_PLANCK_SI/tpi/HARTREE_SI
  REAL(DP), PARAMETER :: AU_PS            = AU_SEC * 1.0D+12
  REAL(DP), PARAMETER :: AU_GPA           = HARTREE_SI / BOHR_RADIUS_SI ** 3 &
                                            / 1.0D+9 
  REAL(DP), PARAMETER :: RY_KBAR          = 10.0_dp * AU_GPA / 2.0_dp
  !
  REAL(DP), PARAMETER :: DEBYE_SI         = 3.3356409519 * 1.0D-30 ! C*m 
  REAL(DP), PARAMETER :: AU_DEBYE         = ELECTRON_SI * BOHR_RADIUS_SI / &
                                            DEBYE_SI
  REAL(DP), PARAMETER :: eV_to_kelvin = ELECTRONVOLT_SI / K_BOLTZMANN_SI
  REAL(DP), PARAMETER :: ry_to_kelvin = RYDBERG_SI / K_BOLTZMANN_SI
  REAL(DP), PARAMETER :: eps4  = 1.0D-4
  REAL(DP), PARAMETER :: eps6  = 1.0D-6
  REAL(DP), PARAMETER :: eps8  = 1.0D-8
  REAL(DP), PARAMETER :: eps14 = 1.0D-14
  REAL(DP), PARAMETER :: eps16 = 1.0D-16
  REAL(DP), PARAMETER :: eps32 = 1.0D-32
  REAL(DP), PARAMETER :: gsmall = 1.0d-12
  REAL(DP), PARAMETER :: e2 = 2.D0      ! the square of the electron charge
  REAL(DP), PARAMETER :: degspin = 2.D0 ! the number of spins per level
  REAL(DP), PARAMETER :: amconv = AMU_RY
  REAL(DP), PARAMETER :: uakbar = RY_KBAR
  REAL(DP), PARAMETER :: bohr_radius_cm = bohr_radius_si * 100.0
  REAL(DP), PARAMETER :: BOHR_RADIUS_ANGS = bohr_radius_cm * 1.0D8
  REAL(DP), PARAMETER :: ANGSTROM_AU = 1.0/BOHR_RADIUS_ANGS
  REAL(DP), PARAMETER :: DIP_DEBYE = AU_DEBYE
  REAL(DP), PARAMETER :: AU_TERAHERTZ  = AU_PS
  REAL(DP), PARAMETER :: AU_TO_OHMCMM1 = 46000.0D0 ! (ohm cm)^-1
  !

END MODULE constants

!
! Copyright (C) 2001-2005 Quantum-ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!---------------------------------------------------------------------------
MODULE parameters
  !---------------------------------------------------------------------------
  !
  IMPLICIT NONE
  SAVE
  !
  INTEGER, PARAMETER :: &
       ntypx  = 10,     &! max number of different types of atom
       npsx   = ntypx,  &! max number of different PPs (obsolete)
       npk    = 40000,  &! max number of k-points               
       lmaxx  = 3,      &! max non local angular momentum (l=0 to lmaxx)      
       nchix  = 6,      &! max number of atomic wavefunctions per atom
       ndmx   = 2000     ! max number of points in the atomic radial mesh
  !
  INTEGER, PARAMETER :: &
       nbrx = 14,          &! max number of beta functions
       lqmax= 2*lmaxx+1,   &! max number of angular momenta of Q
       nqfx = 8             ! max number of coefficients in Q smoothing
  !
  INTEGER, PARAMETER :: nacx    = 10         ! max number of averaged 
                                             ! quantities saved to the restart
  INTEGER, PARAMETER :: nsx     = ntypx      ! max number of species
  INTEGER, PARAMETER :: natx    = 5000       ! max number of atoms
  INTEGER, PARAMETER :: npkx    = npk        ! max number of K points
  INTEGER, PARAMETER :: ncnsx   = 101        ! max number of constraints
  INTEGER, PARAMETER :: nspinx  = 2          ! max number of spinors
  !
  INTEGER, PARAMETER :: nhclm   = 4  ! max number NH chain length, nhclm can be
                                     ! easily increased since the restart file 
                                     ! should be able to handle it, perhaps
                                     ! better to align nhclm by 4
  !
  INTEGER, PARAMETER :: max_nconstr = 100
  !
  INTEGER, PARAMETER  ::  maxcpu = 2**17  ! Maximum number of CPU
  INTEGER, PARAMETER  ::  maxgrp = 128    ! Maximum number of task-groups
  !
END MODULE parameters

MODULE control_flags
  USE kinds
  USE parameters
  IMPLICIT NONE
  SAVE
  TYPE convergence_criteria
     !
     LOGICAL  :: active
     INTEGER  :: nstep
     REAL(DP) :: ekin
     REAL(DP) :: derho
     REAL(DP) :: force
     !
  END TYPE convergence_criteria
  !
  TYPE ionic_conjugate_gradient
     !
     LOGICAL  :: active
     INTEGER  :: nstepix
     INTEGER  :: nstepex
     REAL(DP) :: ionthr
     REAL(DP) :: elethr
     !
  END TYPE ionic_conjugate_gradient
  !
  CHARACTER(LEN=4) :: program_name = ' '  !  used to control execution flow inside module
  !
  LOGICAL :: tvlocw    = .FALSE. ! write potential to unit 46 (only cp, seldom used)
  LOGICAL :: trhor     = .FALSE. ! read rho from      unit 47 (only cp, seldom used)
  LOGICAL :: trhow     = .FALSE. ! CP code, write rho to restart dir
  !
  LOGICAL :: tsde          = .FALSE. ! electronic steepest descent
  LOGICAL :: tzeroe        = .FALSE. ! set to zero the electronic velocities
  LOGICAL :: tfor          = .FALSE. ! move the ions ( calculate forces )
  LOGICAL :: tsdp          = .FALSE. ! ionic steepest descent
  LOGICAL :: tzerop        = .FALSE. ! set to zero the ionic velocities
  LOGICAL :: tprnfor       = .FALSE. ! print forces to standard output
  LOGICAL :: taurdr        = .FALSE. ! read ionic position from standard input
  LOGICAL :: tv0rd         = .FALSE. ! read ionic velocities from standard input
  LOGICAL :: tpre          = .FALSE. ! calculate stress, and (in fpmd) variable cell dynamic
  LOGICAL :: thdyn         = .FALSE. ! variable-cell dynamics (only cp)
  LOGICAL :: tsdc          = .FALSE. ! cell geometry steepest descent
  LOGICAL :: tzeroc        = .FALSE. ! set to zero the cell geometry velocities
  LOGICAL :: tstress       = .FALSE. ! print stress to standard output
  LOGICAL :: tortho        = .FALSE. ! use iterative orthogonalization 
  LOGICAL :: tconjgrad     = .FALSE. ! use conjugate gradient electronic minimization
  LOGICAL :: timing        = .FALSE. ! print out timing information
  LOGICAL :: memchk        = .FALSE. ! check for memory leakage
  LOGICAL :: tprnsfac      = .FALSE. ! print out structure factor 
  LOGICAL :: toptical      = .FALSE. ! print out optical properties
  LOGICAL :: tcarpar       = .FALSE. ! tcarpar is set TRUE for a "pure" Car Parrinello simulation
  LOGICAL :: tdamp         = .FALSE. ! Use damped dinamics for electrons
  LOGICAL :: tdampions     = .FALSE. ! Use damped dinamics for electrons
  LOGICAL :: tatomicwfc    = .FALSE. ! Use atomic wavefunctions as starting guess for ch. density
  LOGICAL :: tscreen       = .FALSE. ! Use screened coulomb potentials for cluster calculations
  LOGICAL :: twfcollect    = .FALSE. ! Collect wave function in the restart file at the end of run.
  LOGICAL :: tuspp         = .FALSE. ! Ultra-soft pseudopotential are being used
  INTEGER :: printwfc      = -1      ! Print wave functions, temporarely used only by ensemble-dft
  LOGICAL :: force_pairing = .FALSE. ! ...   Force pairing
  LOGICAL :: tchi2         = .FALSE. ! Compute Chi^2
  !
  TYPE (convergence_criteria) :: tconvthrs
                              !  thresholds used to check GS convergence 
  !
  ! ... Ionic vs Electronic step frequency
  ! ... When "ion_nstep > 1" and "electron_dynamics = 'md' | 'sd' ", ions are 
  ! ... propagated every "ion_nstep" electronic step only if the electronic 
  ! ... "ekin" is lower than "ekin_conv_thr"
  !
  LOGICAL :: tionstep = .FALSE.
  INTEGER :: nstepe   = 1  
                            !  parameters to control how many electronic steps 
                            !  between ions move

  LOGICAL :: tsteepdesc = .FALSE.
                            !  parameters for electronic steepest desceent

  TYPE (ionic_conjugate_gradient) :: tconjgrad_ion
                            !  conjugate gradient for ionic minimization

  INTEGER :: nbeg   = 0 ! internal code for initialization ( -1, 0, 1, 2, .. )
  INTEGER :: ndw    = 0 !
  INTEGER :: ndr    = 0 !
  INTEGER :: nomore = 0 !
  INTEGER :: iprint = 0 ! print output every iprint step
  INTEGER :: isave  = 0 ! write restart to ndr unit every isave step
  INTEGER :: nv0rd  = 0 !
  INTEGER :: iprsta = 0 ! output verbosity (increasing from 0 to infinity)
  !
  ! ... .TRUE. if only gamma point is used
  !
  LOGICAL :: gamma_only = .TRUE.
  !
  LOGICAL :: tnewnfi = .FALSE.
  INTEGER :: newnfi  = 0
  !
  ! This variable is used whenever a timestep change is requested
  !
  REAL(DP) :: dt_old = -1.0D0
  !
  ! ... Wave function randomization
  !
  LOGICAL  :: trane = .FALSE.
  REAL(DP) :: ampre = 0.D0
  !
  ! ... Ionic position randomization
  !
  LOGICAL  :: tranp(nsx) = .FALSE.
  REAL(DP) :: amprp(nsx) = 0.D0
  !
  ! ... Read the cell from standard input
  !
  LOGICAL :: tbeg = .FALSE.
  !
  ! ... This flags control the calculation of the Dipole Moments
  !
  LOGICAL :: tdipole = .FALSE.
  !
  ! ... Flags that controls DIIS electronic minimization
  !
  LOGICAL :: t_diis        = .FALSE.
  LOGICAL :: t_diis_simple = .FALSE.
  LOGICAL :: t_diis_rot    = .FALSE.
  !
  ! ... Flag controlling the Nose thermostat for electrons
  !
  LOGICAL :: tnosee = .FALSE.
  !
  ! ... Flag controlling the Nose thermostat for the cell
  !
  LOGICAL :: tnoseh = .FALSE.
  !
  ! ... Flag controlling the Nose thermostat for ions
  !
  LOGICAL  :: tnosep = .FALSE.
  LOGICAL  :: tcap   = .FALSE.
  LOGICAL  :: tcp    = .FALSE.
  REAL(DP) :: tolp   = 0.D0   !  tolerance for temperature variation
  !
  REAL(DP), PUBLIC :: &
       ekin_conv_thr = 0.D0, &!  conv. threshold for fictitious e. kinetic energy
       etot_conv_thr = 0.D0, &!  conv. threshold for DFT energy
       forc_conv_thr = 0.D0   !  conv. threshold for atomic forces
  INTEGER, PUBLIC :: &
       ekin_maxiter = 100,   &!  max number of iter. for ekin convergence
       etot_maxiter = 100,   &!  max number of iter. for etot convergence
       forc_maxiter = 100     !  max number of iter. for atomic forces conv.
  !
  ! ... Several variables controlling the run ( used mainly in PW calculations )
  !
  ! ... logical flags controlling the execution
  !
  LOGICAL, PUBLIC :: &
    lfixatom,           &! if .TRUE. some atom is kept fixed
    lscf,               &! if .TRUE. the calc. is selfconsistent
    lbfgs,              &! if .TRUE. the calc. is a relaxation based on new BFGS scheme
    lmd,                &! if .TRUE. the calc. is a dynamics
    lmetadyn,           &! if .TRUE. the calc. is a meta-dynamics
    lpath,              &! if .TRUE. the calc. is a path optimizations
    lneb,               &! if .TRUE. the calc. is NEB dynamics
    lsmd,               &! if .TRUE. the calc. is string dynamics
    lwf,                &! if .TRUE. the calc. is with wannier functions
    lphonon,            &! if .TRUE. the calc. is phonon
    lbands,             &! if .TRUE. the calc. is band structure
    lconstrain,         &! if .TRUE. the calc. is constraint
    ldamped,            &! if .TRUE. the calc. is a damped dynamics
    lrescale_t,         &! if .TRUE. the ionic temperature is rescaled
    langevin_rescaling, &! if .TRUE. the ionic dynamics is overdamped Langevin
    lcoarsegrained,     &! if .TRUE. a coarse-grained phase-space is used
    restart              ! if .TRUE. restart from results of a preceding run
  !
  LOGICAL, PUBLIC :: &
    remove_rigid_rot     ! if .TRUE. the total torque acting on the atoms is
                         ! removed
  !
  ! ... pw self-consistency
  !
  INTEGER, PUBLIC :: &
    ngm0,             &! used in mix_rho
    niter,            &! the maximum number of iteration
    nmix,             &! the number of iteration kept in the history
    imix               ! the type of mixing (0=plain,1=TF,2=local-TF)
  REAL(DP), PUBLIC  :: &
    mixing_beta,      &! the mixing parameter
    tr2                ! the convergence threshold for potential
  LOGICAL, PUBLIC :: &
    conv_elec          ! if .TRUE. electron convergence has been reached
  !
  ! ... pw diagonalization
  !
  REAL(DP), PUBLIC  :: &
    ethr               ! the convergence threshold for eigenvalues  
  INTEGER, PUBLIC :: &
    david,            &! used on Davidson diagonalization
    isolve,           &! Davidson or CG or DIIS diagonalization
    max_cg_iter,      &! maximum number of iterations in a CG di
    diis_buff,        &! dimension of the buffer in diis
    diis_ndim          ! dimension of reduced basis in DIIS
  LOGICAL, PUBLIC :: &
    diago_full_acc     ! if true all the empty eigenvalues have the same
                       ! accuracy of the occupied ones
  !
  ! ... wfc and rho extrapolation
  !
  REAL(DP), PUBLIC  :: &
    alpha0,           &! the mixing parameters for the extrapolation
    beta0              ! of the starting potential
  INTEGER, PUBLIC :: &
    history,          &! number of old steps available for potential updating
    pot_order,        &! type of potential updating ( see update_pot )
    wfc_order          ! type of wavefunctions updating ( see update_pot )
  !
  ! ... ionic dynamics
  !
  INTEGER, PUBLIC :: &
    nstep,            &! number of ionic steps
    istep = 0          ! current ionic step  
  LOGICAL, PUBLIC :: &
    conv_ions          ! if .TRUE. ionic convergence has been reached
  REAL(DP), PUBLIC  :: &
    upscale            ! maximum reduction of convergence threshold
  !
  ! ... system's symmetries
  !
  LOGICAL, PUBLIC :: &
    nosym,            &! if .TRUE. no symmetry is used
    noinv = .FALSE.    ! if .TRUE. eliminates inversion symmetry
  !
  ! ... phonon calculation
  !
  INTEGER, PUBLIC :: &
    modenum            ! for single mode phonon calculation
  !
  ! ... printout control
  !
  LOGICAL, PUBLIC :: &
    reduce_io          ! if .TRUE. reduce the I/O to the strict minimum
  INTEGER, PUBLIC :: &
    iverbosity         ! type of printing ( 0 few, 1 all )
  LOGICAL, PUBLIC :: &
    use_para_diago = .FALSE.  ! if .TRUE. a parallel Householder algorithm 
  INTEGER, PUBLIC :: &
    para_diago_dim = 0        ! minimum matrix dimension above which a parallel
  INTEGER  :: ortho_max = 0    ! maximum number of iterations in routine ortho
  REAL(DP) :: ortho_eps = 0.D0 ! threshold for convergence in routine ortho
  LOGICAL, PUBLIC :: &
    use_task_groups = .FALSE.  ! if TRUE task groups parallelization is used
  INTEGER, PUBLIC :: iesr = 1
  LOGICAL,          PUBLIC :: tvhmean = .FALSE.  
  REAL(DP),         PUBLIC :: vhrmin = 0.0d0
  REAL(DP),         PUBLIC :: vhrmax = 1.0d0
  CHARACTER(LEN=1), PUBLIC :: vhasse = 'Z'
  LOGICAL,          PUBLIC :: tprojwfc = .FALSE.
  CONTAINS
    SUBROUTINE fix_dependencies()
    END SUBROUTINE fix_dependencies
    SUBROUTINE check_flags()
    END SUBROUTINE check_flags
END MODULE control_flags

!
! Copyright (C) 2002 FPMD group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!

!=----------------------------------------------------------------------------=!
   MODULE gvecw
!=----------------------------------------------------------------------------=!
     USE kinds, ONLY: DP

     IMPLICIT NONE
     SAVE

     ! ...   G vectors less than the wave function cut-off ( ecutwfc )
     INTEGER :: ngw  = 0  ! local number of G vectors
     INTEGER :: ngwt = 0  ! in parallel execution global number of G vectors,
                       ! in serial execution this is equal to ngw
     INTEGER :: ngwl = 0  ! number of G-vector shells up to ngw
     INTEGER :: ngwx = 0  ! maximum local number of G vectors
     INTEGER :: ng0  = 0  ! first G-vector with nonzero modulus
                       ! needed in the parallel case (G=0 is on one node only!)

     REAL(DP) :: ecutw = 0.0d0
     REAL(DP) :: gcutw = 0.0d0

     !   values for costant cut-off computations

     REAL(DP) :: ecfix = 0.0d0     ! value of the constant cut-off
     REAL(DP) :: ecutz = 0.0d0     ! height of the penalty function (above ecfix)
     REAL(DP) :: ecsig = 0.0d0     ! spread of the penalty function around ecfix
     LOGICAL   :: tecfix = .FALSE.  ! .TRUE. if constant cut-off is in use

     ! augmented cut-off for k-point calculation

     REAL(DP) :: ekcut = 0.0d0  
     REAL(DP) :: gkcut = 0.0d0
    
     ! array of G vectors module plus penalty function for constant cut-off 
     ! simulation.
     !
     ! ggp = g + ( agg / tpiba**2 ) * ( 1 + erf( ( tpiba2 * g - e0gg ) / sgg ) )

     REAL(DP), ALLOCATABLE, TARGET :: ggp(:)

   CONTAINS

     SUBROUTINE deallocate_gvecw
       IF( ALLOCATED( ggp ) ) DEALLOCATE( ggp )
     END SUBROUTINE deallocate_gvecw

!=----------------------------------------------------------------------------=!
   END MODULE gvecw
!=----------------------------------------------------------------------------=!

!=----------------------------------------------------------------------------=!
   MODULE gvecs
!=----------------------------------------------------------------------------=!
     USE kinds, ONLY: DP

     IMPLICIT NONE
     SAVE

     ! ...   G vectors less than the smooth grid cut-off ( ? )
     INTEGER :: ngs  = 0  ! local number of G vectors
     INTEGER :: ngst = 0  ! in parallel execution global number of G vectors,
                       ! in serial execution this is equal to ngw
     INTEGER :: ngsl = 0  ! number of G-vector shells up to ngw
     INTEGER :: ngsx = 0  ! maximum local number of G vectors

     INTEGER, ALLOCATABLE :: nps(:), nms(:)

     REAL(DP) :: ecuts = 0.0d0
     REAL(DP) :: gcuts = 0.0d0

     REAL(DP) :: dual = 0.0d0
     LOGICAL   :: doublegrid = .FALSE.

   CONTAINS

     SUBROUTINE deallocate_gvecs()
       IF( ALLOCATED( nps ) ) DEALLOCATE( nps )
       IF( ALLOCATED( nms ) ) DEALLOCATE( nms )
     END SUBROUTINE deallocate_gvecs

!=----------------------------------------------------------------------------=!
   END MODULE gvecs
!=----------------------------------------------------------------------------=!

  MODULE electrons_base
      USE kinds, ONLY: DP
      IMPLICIT NONE
      SAVE

      INTEGER :: nbnd       = 0    !  number electronic bands, each band contains
                                   !  two spin states
      INTEGER :: nbndx      = 0    !  array dimension nbndx >= nbnd
      INTEGER :: nspin      = 0    !  nspin = number of spins (1=no spin, 2=LSDA)
      INTEGER :: nel(2)     = 0    !  number of electrons (up, down)
      INTEGER :: nelt       = 0    !  total number of electrons ( up + down )
      INTEGER :: nupdwn(2)  = 0    !  number of states with spin up (1) and down (2)
      INTEGER :: iupdwn(2)  = 0    !  first state with spin (1) and down (2)
      INTEGER :: nudx       = 0    !  max (nupdw(1),nupdw(2))
      INTEGER :: nbsp       = 0    !  total number of electronic states 
                                   !  (nupdwn(1)+nupdwn(2))
      INTEGER :: nbspx      = 0    !  array dimension nbspx >= nbsp

      LOGICAL :: telectrons_base_initval = .FALSE.
      LOGICAL :: keep_occ = .FALSE.  ! if .true. when reading restart file keep 
                                     ! the occupations calculated in initval

      REAL(DP), ALLOCATABLE :: f(:)   ! occupation numbers ( at gamma )
      REAL(DP) :: qbac = 0.0d0        ! background neutralizing charge
      INTEGER, ALLOCATABLE :: ispin(:) ! spin of each state
!
!------------------------------------------------------------------------------!
  CONTAINS
!------------------------------------------------------------------------------!


    SUBROUTINE electrons_base_initval( zv_ , na_ , nsp_ , nelec_ , nelup_ , neldw_ , nbnd_ , &
               nspin_ , occupations_ , f_inp, tot_charge_, multiplicity_, tot_magnetization_ )
      REAL(DP),         INTENT(IN) :: zv_ (:), tot_charge_
      REAL(DP),         INTENT(IN) :: nelec_ , nelup_ , neldw_
      REAL(DP),         INTENT(IN) :: f_inp(:,:)
      INTEGER,          INTENT(IN) :: na_ (:) , nsp_, multiplicity_, tot_magnetization_
      INTEGER,          INTENT(IN) :: nbnd_ , nspin_
      CHARACTER(LEN=*), INTENT(IN) :: occupations_
    END SUBROUTINE electrons_base_initval


    subroutine set_nelup_neldw ( nelec_, nelup_, neldw_, tot_magnetization_, &
         multiplicity_)
      !
      REAL (KIND=DP), intent(IN)    :: nelec_
      REAL (KIND=DP), intent(INOUT) :: nelup_, neldw_
      INTEGER,        intent(IN)    :: tot_magnetization_, multiplicity_
    end subroutine set_nelup_neldw

!----------------------------------------------------------------------------


    SUBROUTINE deallocate_elct()
      IF( ALLOCATED( f ) ) DEALLOCATE( f )
      IF( ALLOCATED( ispin ) ) DEALLOCATE( ispin )
      telectrons_base_initval = .FALSE.
      RETURN
    END SUBROUTINE deallocate_elct


!------------------------------------------------------------------------------!
  END MODULE electrons_base
!------------------------------------------------------------------------------!



!------------------------------------------------------------------------------!
  MODULE electrons_nose
!------------------------------------------------------------------------------!

      USE kinds, ONLY: DP
!
      IMPLICIT NONE
      SAVE

      REAL(DP) :: fnosee   = 0.0d0   !  frequency of the thermostat ( in THz )
      REAL(DP) :: qne      = 0.0d0   !  mass of teh termostat
      REAL(DP) :: ekincw   = 0.0d0   !  kinetic energy to be kept constant

      REAL(DP) :: xnhe0   = 0.0d0   
      REAL(DP) :: xnhep   = 0.0d0   
      REAL(DP) :: xnhem   = 0.0d0   
      REAL(DP) :: vnhe    = 0.0d0
  CONTAINS
  subroutine electrons_nose_init( ekincw_ , fnosee_ )
     REAL(DP), INTENT(IN) :: ekincw_, fnosee_
  end subroutine electrons_nose_init


  function electrons_nose_nrg( xnhe0, vnhe, qne, ekincw )
    real(8) :: electrons_nose_nrg
    real(8), intent(in) :: xnhe0, vnhe, qne, ekincw
    electrons_nose_nrg = 0.0
  end function electrons_nose_nrg

  subroutine electrons_nose_shiftvar( xnhep, xnhe0, xnhem )
    implicit none
    real(8), intent(out) :: xnhem
    real(8), intent(inout) :: xnhe0
    real(8), intent(in) :: xnhep
  end subroutine electrons_nose_shiftvar

  subroutine electrons_nosevel( vnhe, xnhe0, xnhem, delt )
    implicit none
    real(8), intent(inout) :: vnhe
    real(8), intent(in) :: xnhe0, xnhem, delt 
  end subroutine electrons_nosevel

  subroutine electrons_noseupd( xnhep, xnhe0, xnhem, delt, qne, ekinc, ekincw, vnhe )
    implicit none
    real(8), intent(out) :: xnhep, vnhe
    real(8), intent(in) :: xnhe0, xnhem, delt, qne, ekinc, ekincw
  end subroutine electrons_noseupd


  SUBROUTINE electrons_nose_info()
  END SUBROUTINE electrons_nose_info
  END MODULE electrons_nose

module cvan
  use parameters, only: nsx
  implicit none
  save
  integer nvb, ish(nsx)
  integer, allocatable:: indlm(:,:)
contains
  subroutine allocate_cvan( nind, ns )
    integer, intent(in) :: nind, ns
  end subroutine allocate_cvan

  subroutine deallocate_cvan( )
  end subroutine deallocate_cvan

end module cvan

  MODULE cell_base
      USE kinds, ONLY : DP
      IMPLICIT NONE
      SAVE
        REAL(DP) :: alat = 0.0d0
        REAL(DP) :: celldm(6) = (/ 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0 /)
        REAL(DP) :: a1(3) = (/ 0.0d0, 0.0d0, 0.0d0 /)
        REAL(DP) :: a2(3) = (/ 0.0d0, 0.0d0, 0.0d0 /)
        REAL(DP) :: a3(3) = (/ 0.0d0, 0.0d0, 0.0d0 /)
        REAL(DP) :: b1(3) = (/ 0.0d0, 0.0d0, 0.0d0 /)
        REAL(DP) :: b2(3) = (/ 0.0d0, 0.0d0, 0.0d0 /)
        REAL(DP) :: b3(3) = (/ 0.0d0, 0.0d0, 0.0d0 /)
        REAL(DP) :: ainv(3,3) = 0.0d0
        REAl(DP) :: omega = 0.0d0  !  volume of the simulation cell
        REAL(DP) :: tpiba  = 0.0d0   !  = 2 PI / alat
        REAL(DP) :: tpiba2 = 0.0d0   !  = ( 2 PI / alat ) ** 2
        REAL(DP) :: at(3,3) = RESHAPE( (/ 0.0d0 /), (/ 3, 3 /), (/ 0.0d0 /) )
        REAL(DP) :: bg(3,3) = RESHAPE( (/ 0.0d0 /), (/ 3, 3 /), (/ 0.0d0 /) )
        INTEGER          :: ibrav      ! index of the bravais lattice
        CHARACTER(len=9) :: symm_type  ! 'cubic' or 'hexagonal' when ibrav=0
        REAL(DP) :: h(3,3)    = 0.0d0 ! simulation cell at time t 
        REAL(DP) :: hold(3,3) = 0.0d0 ! simulation cell at time t-delt
        REAL(DP) :: hnew(3,3) = 0.0d0 ! simulation cell at time t+delt
        REAL(DP) :: velh(3,3) = 0.0d0 ! simulation cell velocity
        REAL(DP) :: deth      = 0.0d0 ! determinant of h ( cell volume )
        INTEGER   :: iforceh(3,3) = 1  ! if iforceh( i, j ) = 0 then h( i, j ) 
        LOGICAL   :: thdiag = .FALSE.  ! True if only cell diagonal elements 
        REAL(DP) :: wmass = 0.0d0     ! cell fictitious mass
        REAL(DP) :: press = 0.0d0     ! external pressure 
        REAL(DP) :: frich  = 0.0d0    ! firction parameter for cell damped dynamics
        REAL(DP) :: greash = 1.0d0    ! greas parameter for damped dynamics
        LOGICAL :: tcell_base_init = .FALSE.
  CONTAINS
        SUBROUTINE updatecell(box_tm1, box_t0, box_tp1)
          integer :: box_tm1, box_t0, box_tp1
        END SUBROUTINE updatecell
        SUBROUTINE dgcell( gcdot, box_tm1, box_t0, delt )
          REAL(DP), INTENT(OUT) :: GCDOT(3,3)
          REAL(DP), INTENT(IN) :: delt
          integer, intent(in) :: box_tm1, box_t0
        END SUBROUTINE dgcell

        SUBROUTINE cell_init_ht( box, ht )
          integer :: box
          REAL(DP) :: ht(3,3)
        END SUBROUTINE cell_init_ht

        SUBROUTINE cell_init_a( box, a1, a2, a3 )
          integer :: box
          REAL(DP) :: a1(3), a2(3), a3(3)
        END SUBROUTINE cell_init_a

        SUBROUTINE r_to_s1 (r,s,box)
          REAL(DP), intent(out) ::  S(3)
          REAL(DP), intent(in) :: R(3)
          integer, intent(in) :: box
        END SUBROUTINE r_to_s1

        SUBROUTINE r_to_s3 ( r, s, na, nsp, hinv )
          REAL(DP), intent(out) ::  S(:,:)
          INTEGER, intent(in) ::  na(:), nsp
          REAL(DP), intent(in) :: R(:,:)
          REAL(DP), intent(in) :: hinv(:,:)    ! hinv = TRANSPOSE( box%m1 )
          integer :: i, j, ia, is, isa
          isa = 0
          DO is = 1, nsp
            DO ia = 1, na(is)
              isa = isa + 1
              DO I=1,3
                S(I,isa) = 0.D0
                DO J=1,3
                  S(I,isa) = S(I,isa) + R(J,isa)*hinv(i,j)
                END DO
              END DO
            END DO
          END DO
          RETURN
        END SUBROUTINE r_to_s3

!------------------------------------------------------------------------------!

        SUBROUTINE r_to_s1b ( r, s, hinv )
          REAL(DP), intent(out) ::  S(:)
          REAL(DP), intent(in) :: R(:)
          REAL(DP), intent(in) :: hinv(:,:)    ! hinv = TRANSPOSE( box%m1 )
          integer :: i, j
          DO I=1,3
            S(I) = 0.D0
            DO J=1,3
              S(I) = S(I) + R(J)*hinv(i,j)
            END DO
          END DO
          RETURN
        END SUBROUTINE r_to_s1b


        SUBROUTINE s_to_r1 (S,R,box)
          REAL(DP), intent(in) ::  S(3)
          REAL(DP), intent(out) :: R(3)
          integer, intent(in) :: box
        END SUBROUTINE s_to_r1

        SUBROUTINE s_to_r1b (S,R,h)
          REAL(DP), intent(in) ::  S(3)
          REAL(DP), intent(out) :: R(3)
          REAL(DP), intent(in) :: h(:,:)    ! h = TRANSPOSE( box%a )
        END SUBROUTINE s_to_r1b

        SUBROUTINE s_to_r3 ( S, R, na, nsp, h )
          REAL(DP), intent(in) ::  S(:,:)
          INTEGER, intent(in) ::  na(:), nsp
          REAL(DP), intent(out) :: R(:,:)
          REAL(DP), intent(in) :: h(:,:)    ! h = TRANSPOSE( box%a )
        END SUBROUTINE s_to_r3

      SUBROUTINE gethinv(box)
        IMPLICIT NONE
        integer, INTENT (INOUT) :: box
      END SUBROUTINE gethinv


      FUNCTION get_volume( hmat )
         IMPLICIT NONE
         REAL(DP) :: get_volume
         REAL(DP) :: hmat( 3, 3 )
          get_volume = 4.4
      END FUNCTION get_volume

      FUNCTION pbc(rin,box,nl) RESULT (rout)
        IMPLICIT NONE
        integer :: box
        REAL (DP) :: rin(3)
        REAL (DP) :: rout(3), s(3)
        INTEGER, OPTIONAL :: nl(3)
        rout = 4.4
      END FUNCTION pbc

          SUBROUTINE get_cell_param(box,cell,ang)
          IMPLICIT NONE
          integer, INTENT(in) :: box
          REAL(DP), INTENT(out), DIMENSION(3) :: cell
          REAL(DP), INTENT(out), DIMENSION(3), OPTIONAL :: ang
          END SUBROUTINE get_cell_param

      SUBROUTINE pbcs_components(x1, y1, z1, x2, y2, z2, m)
        USE kinds
        INTEGER, INTENT(IN)  :: M
        REAL(DP),  INTENT(IN)  :: X1,Y1,Z1
        REAL(DP),  INTENT(OUT) :: X2,Y2,Z2
        REAL(DP) MIC
      END SUBROUTINE pbcs_components

      SUBROUTINE pbcs_vectors(v, w, m)
        USE kinds
        INTEGER, INTENT(IN)  :: m
        REAL(DP),  INTENT(IN)  :: v(3)
        REAL(DP),  INTENT(OUT) :: w(3)
        REAL(DP) :: MIC
      END SUBROUTINE pbcs_vectors

  SUBROUTINE cell_base_init( ibrav_ , celldm_ , trd_ht, cell_symmetry, rd_ht, cell_units, &
               a_ , b_ , c_ , cosab, cosac, cosbc, wc_ , total_ions_mass , press_ ,  &
               frich_ , greash_ , cell_dofree )

    IMPLICIT NONE
    INTEGER, INTENT(IN) :: ibrav_
    REAL(DP), INTENT(IN) :: celldm_ (6)
    LOGICAL, INTENT(IN) :: trd_ht
    CHARACTER(LEN=*), INTENT(IN) :: cell_symmetry
    REAL(DP), INTENT(IN) :: rd_ht (3,3)
    CHARACTER(LEN=*), INTENT(IN) :: cell_units
    REAL(DP), INTENT(IN) :: a_ , b_ , c_ , cosab, cosac, cosbc
    CHARACTER(LEN=*), INTENT(IN) :: cell_dofree
    REAL(DP),  INTENT(IN) :: wc_ , frich_ , greash_ , total_ions_mass
    REAL(DP),  INTENT(IN) :: press_  ! external pressure from imput ( GPa )
  END SUBROUTINE cell_base_init


  SUBROUTINE cell_base_reinit( ht )
    REAL(DP), INTENT(IN) :: ht (3,3)
  END SUBROUTINE cell_base_reinit

  SUBROUTINE cell_steepest( hnew, h, delt, iforceh, fcell )
    REAL(DP), INTENT(OUT) :: hnew(3,3)
    REAL(DP), INTENT(IN) :: h(3,3), fcell(3,3)
    INTEGER,      INTENT(IN) :: iforceh(3,3)
    REAL(DP), INTENT(IN) :: delt
  END SUBROUTINE cell_steepest

  SUBROUTINE cell_verlet( hnew, h, hold, delt, iforceh, fcell, frich, tnoseh, hnos )
    REAL(DP), INTENT(OUT) :: hnew(3,3)
    REAL(DP), INTENT(IN) :: h(3,3), hold(3,3), hnos(3,3), fcell(3,3)
    INTEGER,      INTENT(IN) :: iforceh(3,3)
    REAL(DP), INTENT(IN) :: frich, delt
    LOGICAL,      INTENT(IN) :: tnoseh
  END SUBROUTINE cell_verlet

  subroutine cell_hmove( h, hold, delt, iforceh, fcell )
    REAL(DP), intent(out) :: h(3,3)
    REAL(DP), intent(in) :: hold(3,3), fcell(3,3)
    REAL(DP), intent(in) :: delt
    integer, intent(in) :: iforceh(3,3)
  end subroutine cell_hmove

  subroutine cell_force( fcell, ainv, stress, omega, press, wmass )
    REAL(DP), intent(out) :: fcell(3,3)
    REAL(DP), intent(in) :: stress(3,3), ainv(3,3)
    REAL(DP), intent(in) :: omega, press, wmass
  end subroutine cell_force

  subroutine cell_move( hnew, h, hold, delt, iforceh, fcell, frich, tnoseh, vnhh, velh, tsdc )
    REAL(DP), intent(out) :: hnew(3,3)
    REAL(DP), intent(in) :: h(3,3), hold(3,3), fcell(3,3)
    REAL(DP), intent(in) :: vnhh(3,3), velh(3,3)
    integer,      intent(in) :: iforceh(3,3)
    REAL(DP), intent(in) :: frich, delt
    logical,      intent(in) :: tnoseh, tsdc
  end subroutine cell_move

  subroutine cell_gamma( hgamma, ainv, h, velh )
    REAL(DP) :: hgamma(3,3)
    REAL(DP), intent(in) :: ainv(3,3), h(3,3), velh(3,3)
  end subroutine cell_gamma

  subroutine cell_kinene( ekinh, temphh, velh )
    REAL(DP), intent(out) :: ekinh, temphh(3,3)
    REAL(DP), intent(in)  :: velh(3,3)
  end subroutine cell_kinene

  function cell_alat( )
    real(DP) :: cell_alat
    cell_alat = 4.4
  end function cell_alat
   END MODULE cell_base


  MODULE ions_base
      USE kinds,      ONLY : DP
      USE parameters, ONLY : ntypx
      IMPLICIT NONE
      SAVE
      INTEGER :: nsp     = 0
      INTEGER :: na(5) = 0    
      INTEGER :: nax     = 0
      INTEGER :: nat     = 0
      REAL(DP) :: zv(5)    = 0.0d0
      REAL(DP) :: pmass(5) = 0.0d0
      REAL(DP) :: amass(5) = 0.0d0
      REAL(DP) :: rcmax(5) = 0.0d0
      INTEGER,  ALLOCATABLE :: ityp(:)
      REAL(DP), ALLOCATABLE :: tau(:,:)     !  initial positions read from stdin (in bohr)
      REAL(DP), ALLOCATABLE :: vel(:,:)     !  initial velocities read from stdin (in bohr)
      REAL(DP), ALLOCATABLE :: tau_srt(:,:) !  tau sorted by specie in bohr
      REAL(DP), ALLOCATABLE :: vel_srt(:,:) !  vel sorted by specie in bohr
      INTEGER,  ALLOCATABLE :: ind_srt(:)   !  index of tau sorted by specie
      INTEGER,  ALLOCATABLE :: ind_bck(:)   !  reverse of ind_srt
      CHARACTER(LEN=3)      :: atm( 5 ) 
      CHARACTER(LEN=80)     :: tau_units


      INTEGER, ALLOCATABLE :: if_pos(:,:)  ! if if_pos( x, i ) = 0 then  x coordinate of 
                                           ! the i-th atom will be kept fixed
      INTEGER, ALLOCATABLE :: iforce(:,:)  ! if_pos sorted by specie 
      INTEGER :: fixatom   = -1            ! to be removed
      INTEGER :: ndofp     = -1            ! ionic degree of freedom
      INTEGER :: ndfrz     = 0             ! frozen degrees of freedom

      REAL(DP) :: fricp   ! friction parameter for damped dynamics
      REAL(DP) :: greasp  ! friction parameter for damped dynamics
      REAL(DP), ALLOCATABLE :: taui(:,:)
      REAL(DP) :: cdmi(3), cdm(3)
      REAL(DP) :: cdms(3)
      LOGICAL :: tions_base_init = .FALSE.
  CONTAINS
    SUBROUTINE packtau( taup, tau, na, nsp )
      REAL(DP), INTENT(OUT) :: taup( :, : )
      REAL(DP), INTENT(IN) :: tau( :, :, : )
      INTEGER, INTENT(IN) :: na( : ), nsp
    END SUBROUTINE packtau

    SUBROUTINE unpacktau( tau, taup, na, nsp )
      REAL(DP), INTENT(IN) :: taup( :, : )
      REAL(DP), INTENT(OUT) :: tau( :, :, : )
      INTEGER, INTENT(IN) :: na( : ), nsp
    END SUBROUTINE unpacktau

    SUBROUTINE sort_tau( tausrt, isrt, tau, isp, nat, nsp )
      REAL(DP), INTENT(OUT) :: tausrt( :, : )
      INTEGER, INTENT(OUT) :: isrt( : )
      REAL(DP), INTENT(IN) :: tau( :, : )
      INTEGER, INTENT(IN) :: nat, nsp, isp( : )
      INTEGER :: ina( nsp ), na( nsp )
    END SUBROUTINE sort_tau

    SUBROUTINE unsort_tau( tau, tausrt, isrt, nat )
      REAL(DP), INTENT(IN) :: tausrt( :, : )
      INTEGER, INTENT(IN) :: isrt( : )
      REAL(DP), INTENT(OUT) :: tau( :, : )
      INTEGER, INTENT(IN) :: nat
    END SUBROUTINE unsort_tau

    SUBROUTINE ions_base_init( nsp_, nat_, na_, ityp_, tau_, vel_, amass_, &
                               atm_, if_pos_, tau_units_, alat_, a1_, a2_, &
                               a3_, rcmax_ )
      INTEGER,          INTENT(IN) :: nsp_, nat_, na_(:), ityp_(:)
      REAL(DP),         INTENT(IN) :: tau_(:,:)
      REAL(DP),         INTENT(IN) :: vel_(:,:)
      REAL(DP),         INTENT(IN) :: amass_(:)
      CHARACTER(LEN=*), INTENT(IN) :: atm_(:)
      CHARACTER(LEN=*), INTENT(IN) :: tau_units_
      INTEGER,          INTENT(IN) :: if_pos_(:,:)
      REAL(DP),         INTENT(IN) :: alat_, a1_(3), a2_(3), a3_(3)
      REAL(DP),         INTENT(IN) :: rcmax_(:)
    END SUBROUTINE ions_base_init

    SUBROUTINE deallocate_ions_base()
    END SUBROUTINE deallocate_ions_base

    SUBROUTINE ions_vel3( vel, taup, taum, na, nsp, dt )
      REAL(DP) :: vel(:,:), taup(:,:), taum(:,:)
      INTEGER :: na(:), nsp
      REAL(DP) :: dt
    END SUBROUTINE ions_vel3

    SUBROUTINE ions_vel2( vel, taup, taum, nat, dt )
      REAL(DP) :: vel(:,:), taup(:,:), taum(:,:)
      INTEGER :: nat
      REAL(DP) :: dt
    END SUBROUTINE ions_vel2

    SUBROUTINE cofmass1( tau, pmass, na, nsp, cdm )
      REAL(DP), INTENT(IN) :: tau(:,:,:), pmass(:)
      REAL(DP), INTENT(OUT) :: cdm(3)
      INTEGER, INTENT(IN) :: na(:), nsp
    END SUBROUTINE cofmass1

    SUBROUTINE cofmass2( tau, pmass, na, nsp, cdm )
      REAL(DP), INTENT(IN) :: tau(:,:), pmass(:)
      REAL(DP), INTENT(OUT) :: cdm(3)
      INTEGER, INTENT(IN) :: na(:), nsp
    END SUBROUTINE cofmass2

      SUBROUTINE randpos(tau, na, nsp, tranp, amprp, hinv, ifor )
         REAL(DP) :: hinv(3,3)
         REAL(DP) :: tau(:,:)
         INTEGER, INTENT(IN) :: ifor(:,:), na(:), nsp
         LOGICAL, INTENT(IN) :: tranp(:)
         REAL(DP), INTENT(IN) :: amprp(:)
         REAL(DP) :: oldp(3), rand_disp(3), rdisp(3)

       END SUBROUTINE randpos

  SUBROUTINE ions_kinene( ekinp, vels, na, nsp, h, pmass )
    REAL(DP), intent(out) :: ekinp     !  ionic kinetic energy
    REAL(DP), intent(in) :: vels(:,:)  !  scaled ionic velocities
    REAL(DP), intent(in) :: pmass(:)   !  ionic masses
    REAL(DP), intent(in) :: h(:,:)     !  simulation cell
    integer, intent(in) :: na(:), nsp
    integer :: i, j, is, ia, ii, isa
  END SUBROUTINE ions_kinene

  subroutine ions_temp( tempp, temps, ekinpr, vels, na, nsp, h, pmass, ndega, nhpdim, atm2nhp, ekin2nhp )
    REAL(DP), intent(out) :: ekinpr, tempp
    REAL(DP), intent(out) :: temps(:)
    REAL(DP), intent(out) :: ekin2nhp(:)
    REAL(DP), intent(in)  :: vels(:,:)
    REAL(DP), intent(in)  :: pmass(:)
    REAL(DP), intent(in)  :: h(:,:)
    integer,        intent(in)  :: na(:), nsp, ndega, nhpdim, atm2nhp(:)
  end subroutine ions_temp

  subroutine ions_thermal_stress( stress, pmass, omega, h, vels, nsp, na )
    REAL(DP), intent(inout) :: stress(3,3)
    REAL(DP), intent(in)  :: pmass(:), omega, h(3,3), vels(:,:)
    integer, intent(in) :: nsp, na(:)
    integer :: i, j, is, ia, isa
  end subroutine ions_thermal_stress

  subroutine ions_vrescal( tcap, tempw, tempp, taup, tau0, taum, na, nsp, fion, iforce, &
                           pmass, delt )
    logical, intent(in) :: tcap
    REAL(DP), intent(inout) :: taup(:,:)
    REAL(DP), intent(in) :: tau0(:,:), taum(:,:), fion(:,:)
    REAL(DP), intent(in) :: delt, pmass(:), tempw, tempp
    integer, intent(in) :: na(:), nsp
    integer, intent(in) :: iforce(:,:)
  end subroutine ions_vrescal
  subroutine ions_shiftvar( varp, var0, varm )
    REAL(DP), intent(in) :: varp
    REAL(DP), intent(out) :: varm, var0
  end subroutine ions_shiftvar
   SUBROUTINE cdm_displacement( dis, tau )
      REAL(DP) :: dis
      REAL(DP) :: tau
   END SUBROUTINE cdm_displacement
   SUBROUTINE ions_displacement( dis, tau )
      REAL (DP), INTENT(OUT) :: dis
      REAL (DP), INTENT(IN)  :: tau
   END SUBROUTINE ions_displacement
  END MODULE ions_base
