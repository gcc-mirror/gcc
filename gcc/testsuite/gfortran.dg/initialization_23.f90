! { dg-do compile }
!
! PR 40875: The error was missed and an ICE ensued.
!
! Contributed by Michael Richmond <michael.a.richmond@nasa.gov>
!
    MODULE cdf_aux_mod
      PUBLIC
      TYPE :: one_parameter
        CHARACTER :: name
      END TYPE one_parameter
      CHARACTER, PARAMETER :: the_alpha = one_parameter('c') ! { dg-error "Cannot convert TYPE" }
      CHARACTER, PARAMETER :: the_beta = (/one_parameter('c')/) ! { dg-error "Incompatible ranks" }
    END MODULE cdf_aux_mod
