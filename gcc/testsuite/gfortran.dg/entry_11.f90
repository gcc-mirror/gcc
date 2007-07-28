! { dg-do compile }
! PR31609 module that calls a contained function with an ENTRY point
! Test case derived from the PR

MODULE ksbin1_aux_mod
  CONTAINS
    SUBROUTINE sub
    i = k()
    END SUBROUTINE sub
    FUNCTION j () 
      print *, "in j"    
    j = 111 
    ENTRY k () 
      print *, "in k"    
    k = 222
    END FUNCTION j
END MODULE ksbin1_aux_mod

program testit
  use ksbin1_aux_mod
  l = j()
  print *, l
  l = k()
  print *, l
end program testit