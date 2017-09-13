! { dg-do run }
!
! Fixes problem setting CHARACTER KIND expressions in PDT components
! and resolution of intrinsic functions and numeric expressions.
!
! Contributed by FortranFan on clf thread "Parameterized Derived Types
! make first appearance in gfortran 8.0.0"
!
program p
   use, intrinsic :: iso_fortran_env, only : CK => character_kinds
   implicit none
   character(kind = 4), parameter :: c = 'a'
   type :: pdt_t(k,l)
      integer, kind :: k = CK(1)
      integer, len :: l
      character(kind=k,len=l) :: s
   end type
   type(pdt_t(l=12)) :: foo
   type(pdt_t(k = kind (c), l=12)) :: foo_4

   foo%s = "Hello World!"
   if (foo%s .ne. "Hello World!") call abort
   if (KIND (foo%s) .ne. 1) call abort
   if (len (foo%s) .ne. 12) call abort

   foo_4%s = "Hello World!"
   if (foo_4%s .ne. "Hello World!") call abort
   if (KIND (foo_4%s) .ne. 1) call abort
   if (len (foo_4%s) .ne. 12) call abort
end program
