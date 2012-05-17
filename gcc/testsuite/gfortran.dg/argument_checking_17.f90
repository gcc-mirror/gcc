! { dg-do compile }
!
! PR fortran/47569
!
! Contributed by Jos de Kloe 
!
module teststr
  implicit none
  integer, parameter :: GRH_SIZE = 20, NMAX = 41624
  type strtype
    integer   :: size
    character :: mdr(NMAX)
  end type strtype
contains
  subroutine sub2(string,str_size)
    integer,intent(in)    :: str_size
    character,intent(out) :: string(str_size)
    string(:) = 'a'
  end subroutine sub2
  subroutine sub1(a)
    type(strtype),intent(inout) :: a
    call sub2(a%mdr(GRH_SIZE+1),a%size-GRH_SIZE)
  end subroutine sub1
end module teststr
