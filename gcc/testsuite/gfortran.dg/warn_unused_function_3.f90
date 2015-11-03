! { dg-do compile }
! { dg-options "-Wunused-function" }
!
! PR 67982: Bogus -Wunused-function warning with contained function
!
! Contributed by Joost VandeVondele <Joost dot VandeVondele at mat dot ethz dot ch> 

MODULE base
  INTERFACE 
    SUBROUTINE bar_int()
    END SUBROUTINE 
  END INTERFACE
  PUBLIC hook
  PRIVATE 
  PROCEDURE(bar_int), POINTER :: hook=>NULL()
END MODULE base

MODULE foo
  USE base, ONLY: hook  
  PUBLIC init
  PRIVATE 
CONTAINS
  SUBROUTINE init()
     hook=>bar
  END SUBROUTINE init
  SUBROUTINE bar()
     WRITE(6,*) "In bar"
  END SUBROUTINE 
END MODULE

USE foo, ONLY: init
USE base, ONLY: hook
CALL init()
CALL hook()
END
