!{ dg-do run }

! Contributed by Damian Rouson  <damian@archaeologic.codes>
! Check that PR fortran/85510 links.

module foo
contains
  subroutine bar()
    integer, save :: i[*] = 1
    associate(n=>1)
      if (i[1] /= 1) stop 1
    end associate
  end subroutine
end module

use foo
call bar()
end

