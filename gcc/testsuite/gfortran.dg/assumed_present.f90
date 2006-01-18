! { dg-do compile }
! This tests the fix for the regression PR25785, where line 7 started
! generating an assumed size error.
! Contributed by Dale Ranta  <dir@lanl.gov>
      subroutine my_sio_file_write_common(data_c1)
        character,   intent(in), optional :: data_c1(*)
        if (present(data_c1)) then
        endif
      end subroutine my_sio_file_write_common
