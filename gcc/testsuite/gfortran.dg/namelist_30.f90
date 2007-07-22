! { dg-do compile }
!
! PR fortran/32710 - ICE: namelist and subroutine with the same name
!
! Contributed by Janus Weil <jaydub66 AT gmail DOT com>
!

program x
contains
  subroutine readInput
    integer:: a
    NAMELIST /foo/ a
    read(5,nml=foo)
  end subroutine readInput

  subroutine foo()
  end subroutine

end program
