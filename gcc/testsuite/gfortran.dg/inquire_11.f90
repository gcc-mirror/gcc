! { dg-do compile }
! PR 33055 Runtime error in INQUIRE unit existance with -fdefault-integer-8
! Test case from PR33217 prepared by Jerry DeLisle <jvdelisle@gcc.gnu.org>
MODULE print_it
CONTAINS
  SUBROUTINE i()
    LOGICAL :: qexist
    INQUIRE (UNIT=1, EXIST=qexist)
  END SUBROUTINE i
END MODULE print_it
! { dg-final { cleanup-modules "print_it" } }
