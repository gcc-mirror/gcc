      ! { dg-do compile }
      ! { dg-options "-fdec-structure" }
      !
      ! Test error handling for %FILL
      !
      implicit none

      structure /s/
        integer(2) i /3/
        integer(2) %fill /4/ ! { dg-error "cannot have an initializer" }
        integer(2), pointer :: %fill ! { dg-error "cannot have attributes" }
      end structure

      type t
        integer %fill ! { dg-error "not allowed outside STRUCTURE" }
      endtype

      end
