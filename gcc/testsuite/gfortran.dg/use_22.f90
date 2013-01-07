! { dg-do compile }
!
! PR fortran/55827
! gfortran used to ICE with the call to `tostring' depending on how the
! `tostring' symbol was USE-associated.
!
! Contributed by Lorenz Hüdepohl <bugs@stellardeath.org>

module stringutils
  interface
    pure function strlen(handle) result(len)
      integer, intent(in) :: handle
      integer :: len
    end function
  end interface
end module
module intermediate ! does not die if this module is merged with stringutils
  contains
  function tostring(handle) result(string)
    use stringutils
    integer, intent(in) :: handle
    character(len=strlen(handle)) :: string
  end function
end module
module usage
  contains
  subroutine dies_here(handle)
    use stringutils ! does not die if this unnecessary line is omitted or placed after "use intermediate"
    use intermediate
    integer :: handle
    write(*,*) tostring(handle) ! ICE
  end subroutine
end module


