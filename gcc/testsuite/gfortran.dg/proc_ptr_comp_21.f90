! { dg-do compile }
!
! PR 41242: [4.5 Regression] PPC call rejected (related to user-defined assignment?)
!
! Original test case by Juergen Reuter <reuter@physik.uni-freiburg.de>
! Modified by Janus Weil <janus@gcc.gnu.org>

  type :: nf_t
     procedure(integer), nopass, pointer :: get_n_in
  end type

  interface assignment(=)
     procedure op_assign
  end interface

  type(nf_t) :: prc_lib
  prc_lib = "foobar"
  print *, prc_lib%get_n_in()

contains

  elemental subroutine op_assign (str, ch) ! { dg-warning "Extension: Internal procedure" }
    type(nf_t), intent(out) :: str
    character(len=*), intent(in) :: ch
  end subroutine

end

