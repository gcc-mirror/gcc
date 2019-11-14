! { dg-do run }
! { dg-additional-sources ISO_Fortran_binding_17.c }
! { dg-options "-fcheck=all" }
! { dg-warning "command-line option '-fcheck=all' is valid for Fortran but not for C" "" { target *-*-* } 0 }
!
! PR fortran/92470
!
! https://github.com/j3-fortran/fortran_proposals/issues/57#issuecomment-552680503
!
! Unit Test #: Test-1.F2018-2.7.5
! Author     : FortranFan
! Reference  : The New Features of Fortran 2018, John Reid, August 2, 2018
!              ISO/IEC JTC1/SC22/WG5 N2161
! Description:
! Test item 2.7.5 Fortran subscripting
! void *CFI_address(const CFI_cdesc_t *dv, const CFI_index_t subscripts[]);
! that returns the C address of a scalar or of an element of an array using
! Fortran sub-scripting.
!
   use, intrinsic :: iso_c_binding, only: c_int, c_size_t, c_loc
   implicit none

   integer, parameter :: LB_A = -2
   integer, parameter :: UB_A = 1
   character(len=*), parameter :: fmtg = "(*(g0,1x))"
   character(len=*), parameter :: fmth = "(g0,1x,z0)"

   blk1: block
      interface
         subroutine Csub(a, loc_a_1, invalid_idx) bind(C, name="Csub")
            import :: c_size_t
            type(*), intent(in) :: a(:)
            integer(c_size_t), intent(in), value :: loc_a_1, invalid_idx
         end subroutine
      end interface

      integer(c_int), target :: a( LB_A:UB_A )
      integer(c_size_t) :: loc_a

      print fmtg, "Block 1"

      loc_a = transfer( c_loc(a(lbound(a,dim=1))), mold=loc_a)
      print fmth, "Address of a: ", loc_a

      call Csub(a, loc_a, -1_c_size_t) ! LB starts at 0
      call Csub(a, loc_a, 5_c_size_t)  ! 4 elements + 1
      print *
   end block blk1

   blk2: block
      interface
         subroutine Csub(a, loc_a_1, invalid_idx) bind(C, name="Csub")
            import :: c_int, c_size_t
            integer(kind=c_int), allocatable, intent(in) :: a(:)
            integer(c_size_t), intent(in), value :: loc_a_1, invalid_idx
         end subroutine
      end interface

      integer(c_int), allocatable, target :: a(:)
      integer(c_size_t) :: loc_a

      print fmtg, "Block 2"

      allocate( a( LB_A:UB_A ) )
      loc_a = transfer( c_loc(a(lbound(a,dim=1))), mold=loc_a )
      print fmth, "Address of a: ", loc_a

      call Csub(a, loc_a, LB_A-1_c_size_t)
      call Csub(a, loc_a, UB_A+1_c_size_t)
      print *
   end block blk2
end

! { dg-output "CFI_address: subscripts\\\[0\\\] is out of bounds. For dimension = 0, subscripts = -1, lower_bound = 0, upper bound = 4, extend = 4(\n|\r\n|\r)" }
! { dg-output "CFI_address: subscripts\\\[0\\\] is out of bounds. For dimension = 0, subscripts = 5, lower_bound = 0, upper bound = 4, extend = 4(\n|\r\n|\r).*" }
! { dg-output "CFI_address: subscripts\\\[0\\\] is out of bounds. For dimension = 0, subscripts = -3, lower_bound = -2, upper bound = 6, extend = 4(\n|\r\n|\r)" }
! { dg-output "CFI_address: subscripts\\\[0\\\] is out of bounds. For dimension = 0, subscripts = 2, lower_bound = -2, upper bound = 6, extend = 4(\n|\r\n|\r)" }
