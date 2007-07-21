! PR fortran/32823
! { dg-do compile }
! { dg-final { cleanup-modules "token_module" } }

module token_module

      integer,     parameter :: INT8  = SELECTED_INT_KIND(16)
      integer,     parameter :: REAL8 = SELECTED_REAL_KIND(12)

contains
      subroutine token_allreduce_i8_v(dowhat, array, result, length)


        character(*),  intent(in)    :: dowhat
        integer,       intent(in)    :: length
        integer(INT8), intent(in)    :: array(*)
        integer(INT8), intent(inout) :: result(*)


        real(REAL8) :: copy_r8(length), result_r8(length)


          result(1:length) = int(result_r8(1:length), INT8)


      end subroutine token_allreduce_i8_v

end module token_module
