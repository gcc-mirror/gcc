! { dg-do compile }
! PR32612 gfortran - incorrectly flags error on interface module
! Test case is that of the reporters
   module files_module
      implicit none
          integer, parameter :: REAL8 = SELECTED_REAL_KIND(12)      
      save
      private
      interface my_sio_file_read_common
        module procedure my_sio_file_read_common ! This was rejected before
      end interface
    contains
      subroutine my_sio_file_read_all_i4(serial, data, data_lengths, error)
        logical, intent(in)  :: serial
        integer, intent(out) :: data(*)
        integer, intent(in)  :: data_lengths(0:*)
        integer, intent(out) :: error
        call my_sio_file_read_common(data_lengths, error, data_i4 = data)
      end subroutine my_sio_file_read_all_i4
      subroutine my_sio_file_read_common(data_lengths, error, &
                                         data_i4, &
                                         data_r8)
        integer,     intent(in)  :: data_lengths(0:*)
        integer,     intent(out) :: error
        integer,     intent(out), optional :: data_i4(*)
        real(REAL8), intent(out), optional :: data_r8(*)
          error=0
          data_i4(1)=0
          data_r8(1)=0
      end subroutine my_sio_file_read_common
    end module files_module
