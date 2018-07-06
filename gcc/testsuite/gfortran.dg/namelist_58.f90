! { dg-do run }
! PR40853 Error in namelist IO.
! Test case derived from example given in PR. < jvdelisle@gcc.gnu.org >
program test
  implicit none
  type tao_title_struct
    character(2) justify   
  end type
  type tao_plot_page_struct
    real shape_height_max 
    type (tao_title_struct) title ! Comment this line out and the bug goes away.
    real size(2)
  end type
  type (tao_plot_page_struct) plot_page
  namelist / params / plot_page
  open (10, status="scratch")
  write(10,'(a)')"  &params"
  write(10,'(a)')"  plot_page%size=5 , 2,"
  write(10,'(a)')"/"
  rewind(10)
  read (10, nml = params)
  if (any(plot_page%size .ne. (/ 5, 2 /))) STOP 1
  close (10)
end program

