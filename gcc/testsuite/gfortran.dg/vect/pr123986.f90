! { dg-do compile }
! { dg-additional-options "-Ofast -mavx512f" { target avx512f } }

MODULE bulk_cloud_model_mod
  INTEGER, PARAMETER ::  wp  = KIND(1.0D0)
  INTEGER, PARAMETER ::  nzt = 1
  REAL, PARAMETER ::  df_crit  = 100.0E-6
  TYPE cloud_coefficients
     REAL(wp) a
     REAL(wp) b
  END TYPE 
  TYPE cloud_species_def
     TYPE(cloud_coefficients) cloud
     TYPE(cloud_coefficients) graupel
  END TYPE 
  TYPE(cloud_species_def) cloud_species
CONTAINS
  SUBROUTINE riming_graupel_cloud
    REAL(wp) dc
    REAL(wp) dg
    REAL(wp) xc
    REAL(wp) xg
    DO  k = 1, nzt
       dg = mean_diameter( cloud_species%graupel,  xg )
       dc = mean_diameter( cloud_species%cloud,    xc )
       IF ( dc > dc_crit  .AND.  dg > df_crit )  THEN
          riming_rate_n =   nc_c ( rime_graupel_cloud_delta_n_aa * dg**2 + dg * dc * dc**2 )                                    
       ENDIF
    ENDDO
  END  
  FUNCTION mean_diameter( specie, particle_mean_mass )
    CLASS(cloud_coefficients)   specie
    REAL(wp)   particle_mean_mass
    mean_diameter = specie%a * particle_mean_mass**specie%b
  END  
END 
