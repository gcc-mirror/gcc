! { dg-do run }
! { dg-additional-sources "allocate-errors-c.c dump-descriptors.c" }
! { dg-additional-options "-Wno-error -fcheck=all" }
! { dg-warning "command-line option '-fcheck=all' is valid for Fortran but not for C" "" { target *-*-* } 0 }
!
! This program tests that the CFI_allocate and CFI_deallocate functions
! properly detect invalid arguments.  All the interesting things happen
! in the corresponding C code.
!
! The situation here seems to be that while TS29113 defines error codes for
! these functions, it doesn't actually require the implementation to detect
! those errors by saying the arguments "shall be" such-and-such, e.g. it is
! undefined behavior if they are not.  In gfortran you can enable some
! run-time checking by building with -fcheck=all.

program testit
  use iso_c_binding
  implicit none

  interface
    subroutine ctest () bind (c)
    end subroutine
  end interface

  call ctest ()

end program
