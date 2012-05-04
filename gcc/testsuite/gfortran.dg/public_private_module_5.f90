! { dg-do compile }
! { dg-options "-O3" }
!
! PR fortran/53175
!

MODULE ENERGY_FUNCTION
   IMPLICIT NONE

   TYPE PARAM
      PRIVATE
         INTEGER :: WHICH_VECTOR
   END TYPE PARAM

   INTEGER, PRIVATE :: DIM2
   INTEGER, PRIVATE :: DIM5

   private :: specific
   interface gen
     module procedure  specific
   end interface gen

   CONTAINS

      FUNCTION ENERGY_FUNCTION_CURRENT_ARGS()
         INTEGER, DIMENSION(DIM2) :: ENERGY_FUNCTION_CURRENT_ARGS
      END FUNCTION ENERGY_FUNCTION_CURRENT_ARGS

      FUNCTION ENERGY_FUNCTION_GET_PARAMS()
         TYPE(PARAM), DIMENSION(DIM2) :: ENERGY_FUNCTION_GET_PARAMS
      END FUNCTION ENERGY_FUNCTION_GET_PARAMS   

      function specific()
        character(len=dim5) :: specific
      end function specific
END MODULE ENERGY_FUNCTION

! { dg-final { scan-assembler "__energy_function_MOD_dim2" } }
! { dg-final { scan-assembler "__energy_function_MOD_dim5" } }

