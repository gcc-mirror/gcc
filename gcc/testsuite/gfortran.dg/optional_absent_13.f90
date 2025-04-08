! { dg-do run }
! PR fortran/119656 - wrong code with impure elemental subroutine and interface
!
! Derived from testcase at:
!   https://fortran-lang.discourse.group/t/
!     problem-with-impure-elemental-subroutine-in-interface-with-gfortran/9545

module m2
  implicit none
  interface foo
     module procedure foo_mat
     module procedure foo_df
     module procedure foo_cmat
  end interface foo
contains

  subroutine foo_mat(x, nacf, label)
    real,             intent(in)           :: x(:,:)
    integer,          intent(in)           :: nacf
    character(len=*), intent(in), optional :: label
  end subroutine foo_mat

  impure elemental subroutine foo_df(nacf, outu, xstr)
    integer         , intent(in)           :: nacf
    integer         , intent(in), optional :: outu
    character(len=*), intent(in), optional :: xstr
    if (present(xstr)) then
       if (len (xstr) /= 2) then
          print *,"nacf, len(xstr) =", nacf, len(xstr)
          stop nacf
       end if
    end if
  end subroutine foo_df

  subroutine foo_cmat(x, nacf, label)
    complex,          intent(in)           :: x(:,:)
    integer,          intent(in)           :: nacf
    character(len=*), intent(in), optional :: label
  end subroutine foo_cmat

end module m2

program main
  use m2, only: foo, foo_df
  implicit none
  call foo_df(nacf = 1, xstr="ab")
  call foo   (nacf = 2, xstr="ab")
end program main
