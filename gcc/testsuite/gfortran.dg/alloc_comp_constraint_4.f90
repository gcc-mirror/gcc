! { dg-do compile }
! Tests the fix for PR29422, in which function results
! were not tested for suitability in IO statements.
!
! Contributed by Dominique d'Humieres  <dominiq@lps.ens.fr>
!
Type drv
 Integer :: i
 Integer, allocatable :: arr(:)
End type drv

  print *, fun1 () ! { dg-error "cannot have ALLOCATABLE" }

contains
  Function fun1 ()

    Type(drv) :: fun1
    fun1%i = 10
  end function fun1
end

