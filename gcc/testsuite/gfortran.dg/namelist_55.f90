! { dg-do run }
! { dg-options "-std=legacy" }
!
! PR37707 Namelist read of array of derived type incorrect
! Test case from PR, prepared by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
TYPE geometry
   INTEGER :: nlon,nlat,nlev,projection
   INTEGER :: center,subcenter,process
   REAL    :: west,south,east,north
   REAL    :: dlon,dlat
   REAL    :: polat,polon
   REAL    :: lonc,latc
   REAL    :: projlat,projlat2,projlon
   CHARACTER(LEN=1) :: arakawa ='#'
   INTEGER :: truncx,truncy   ! Spectral truncation
   INTEGER :: cie             ! Flag fort CI (0), CIE gridpoint (1)
                              ! or CIE spectral (-1)
   INTEGER :: nlat_i,nlon_i   ! I length in Y and X direction
   INTEGER :: nlat_e ,nlon_e  ! E length in Y and X direction
   LOGICAL :: do_geo = .true.
END TYPE geometry

TYPE shortkey
   INTEGER           :: PPP !  2. Parameter
   INTEGER           :: NNN ! 12. Gridpoint or spectral field 0 = gridpoint, 1 = spectral
   INTEGER           :: INTPM
   CHARACTER(LEN=16) :: name
END TYPE shortkey
INTEGER, PARAMETER :: maxl       = 200  ! Maximum number of levels to be read from namelist
INTEGER, PARAMETER :: max_atmkey = 10   ! Maximum number of extra fields in the

REAL    :: ahalf(maxl),bhalf(maxl)
TYPE (geometry) :: outgeo ; SAVE outgeo  ! Output geometry

TYPE (shortkey) ::  atmkey(max_atmkey) ; SAVE atmkey
TYPE (shortkey) :: mlevkey(max_atmkey) ; SAVE mlevkey

character*600 :: l = " &NAMINTERP atmkey%ppp = 076,058,062,079, atmkey%nnn = 000,000,000,000, &
                     & atmkey%name ='LIQUID_WATER','SOLID_WATER','SNOW','RAIN', OUTGEO%NLEV=10, &
                     & AHALF=0.,1.,2.,3.,4.,5.,6.,7.,8.,9., BHALF=0.,1.,2.,3.,4.,5.,6.,7.,8.,9., /"

namelist /naminterp/outgeo,ahalf,bhalf,atmkey
print *, outgeo%nlev
read(l,nml=naminterp)
if (outgeo%nlev /= 10) STOP 1
if (any(ahalf(1:10) .ne. [0.,1.,2.,3.,4.,5.,6.,7.,8.,9.])) STOP 2
if (any(bhalf(1:10) .ne. [0.,1.,2.,3.,4.,5.,6.,7.,8.,9.])) STOP 3
if (any(atmkey(1:4)%ppp .ne. [076,058,062,079])) STOP 4
if (any(atmkey(1:4)%nnn .ne. [0,0,0,0])) STOP 5
if (any(atmkey(1:4)%name .ne. ['LIQUID_WATER','SOLID_WATER ','SNOW        ',&
                              &'RAIN        '])) STOP 6
end
